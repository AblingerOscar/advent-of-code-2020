open ActivePatterns
open Lib
open System
open System.Text.RegularExpressions

let getData argv =
    let parseSection (str: string seq) =
        let id =
            match Seq.head str with
            | MatchesRegex "Tile (\d+):" id -> Int32.Parse id
            | _ -> failwithf "could not match id line: '%s'" (Seq.head str)
        (id, str |> Seq.tail |> List.ofSeq)

    readEmptyLineSeparatedSections (getFileName argv)
    |> Seq.map parseSection
    |> Map.ofSeq

type Tile =
    {
        Id: int
        Text: string list
    }

    static member Default = { Id = 0; Text = List.empty }

let rotateTextLeft (text: string list) =
    if text.IsEmpty then
        failwith "rotateTextRight: text is empty"
    else
        [ // assuming that all strings have the same length
            for i in text.[0].Length - 1 .. -1 .. 0 do
                yield
                    text
                    |> List.map (fun str -> str.[i])
                    |> Seq.ofList
                    |> String.Concat
        ]

let flipText (text: string list) =
    if text.IsEmpty then
        failwith "flipText: text is empty"
    else
        List.rev text

let getBottomEdge text = List.head text
let getTopEdge text = List.last text
let getRightEdge (text: string list) = text |> List.map (fun line -> line.[0]) |> String.Concat
let getLeftEdge (text: string list) = text |> List.map (fun line -> line.[line.Length - 1]) |> String.Concat

let getAllVariationsOfText (text: string list) =
    seq {
        yield text // normal text
        // rotated text
        yield text |> rotateTextLeft
        yield text |> rotateTextLeft |> rotateTextLeft
        yield text |> rotateTextLeft |> rotateTextLeft |> rotateTextLeft
        // flipped text (x-axis) in all rotations
        yield text |> flipText
        yield text |> flipText |> rotateTextLeft
        yield text |> flipText |> rotateTextLeft |> rotateTextLeft
        yield text |> flipText |> rotateTextLeft |> rotateTextLeft |> rotateTextLeft
        // flipped text (y-axis) in all rotations
        yield text |> rotateTextLeft |> flipText
        yield text |> rotateTextLeft |> flipText |> rotateTextLeft
        yield text |> rotateTextLeft |> flipText |> rotateTextLeft |> rotateTextLeft
        yield text |> rotateTextLeft |> flipText |> rotateTextLeft |> rotateTextLeft |> rotateTextLeft
    }

// pretty inefficient, but it works
let transformToMatch (getEdge: string list -> string) (edge: string) (text: string list) =
    getAllVariationsOfText text
    |> Seq.tryPick (fun transformedText ->
            if (getEdge transformedText) = edge then
                Some transformedText
            else
                None
        )

//  ===== assembly functions =====

let rec fillInDirection
        (tileIsNext: Tile -> int * string list -> Option<int * string list>)
        (tileMap: Map<int, string list>)
        (assignedTiles: Tile list)
        (unassignedTiles: Set<int>) =
    let currTile = assignedTiles.Head
   
    let nextTile =
        unassignedTiles
        |> Seq.map (fun id -> id, tileMap.[id])
        |> Seq.tryPick (tileIsNext currTile)

    match nextTile with
    | None -> tileMap, assignedTiles
    | Some (id, text) ->
        let newAssignedTiles = ({ Tile.Default with Id = id; Text = text })::assignedTiles

        fillInDirection tileIsNext (tileMap.Add(id, text)) newAssignedTiles (unassignedTiles.Remove id)

let fillRight =
    fillInDirection
        (fun tile (id, text) -> transformToMatch getLeftEdge (getRightEdge tile.Text) text |> Option.map (fun text -> id, text))
let fillLeft =
    fillInDirection
        (fun tile (id, text) -> transformToMatch getRightEdge (getLeftEdge tile.Text) text |> Option.map (fun text -> id, text))

let fillRightAndLeft (tileMap: Map<int, string list>) (assignedTiles: Tile list) (unassignedTiles: Set<int>) =
    let (newTileMap, rowRight) = fillRight tileMap assignedTiles unassignedTiles 
    let newUnassignedTiles = Set.difference unassignedTiles (rowRight |> List.map (fun tile -> tile.Id) |> Set.ofList)
    let (newTileMap, completeRow) = fillLeft newTileMap (List.rev rowRight) newUnassignedTiles 
    (newTileMap, List.rev completeRow)

let rec moveVerticallyFillLeft (rows: Tile list list) (textMap: Map<int, string list>) (unassignedTiles: Set<int>) (getNextStartingTile: Tile -> Map<int, string list> -> Set<int> -> (int * string list) option) = 
    match getNextStartingTile rows.Head.Head textMap unassignedTiles with
    | None -> rows, textMap, unassignedTiles
    | Some (startTileId, startTileText) ->
        let (newTextMap, tileRow) = fillLeft (textMap.Add(startTileId, startTileText)) (List.singleton { Tile.Default with Id = startTileId; Text = startTileText }) (unassignedTiles.Remove startTileId)
        let newUnassignedTiles = Set.difference unassignedTiles (tileRow |> List.map (fun tile -> tile.Id) |> Set.ofList)

        moveVerticallyFillLeft ((List.rev tileRow)::rows) newTextMap newUnassignedTiles getNextStartingTile

let assembleTiles (tileIdsToTexts: Map<int, string list>) =
    // step 1: choose any tile & try to get the entire line of tiles (fill all lefts & rights until impossible)
    let mutable textMap = tileIdsToTexts
    let firstTile = textMap |> Map.toSeq |> Seq.head |> fst
    let mutable unassignedTileIds =
        textMap
        |> Map.toSeq
        |> Seq.map fst
        |> Set.ofSeq
        |> fun set -> set.Remove firstTile

    let (newTextMap, firstRow) = fillRightAndLeft textMap (List.singleton { Tile.Default with Id = firstTile; Text = textMap.[firstTile] }) unassignedTileIds
    textMap <- newTextMap
    unassignedTileIds <- Set.difference unassignedTileIds (firstRow |> List.map (fun tile -> tile.Id) |> Set.ofList)

    // step 2: get all lines above the current one find top left tile & then fill to the right – repeat until we're topmost
    let getTopTile (tile: Tile) (textMap: Map<int, string list>) (unassignedIds: Set<int>) =
        unassignedIds
        |> Seq.map (fun id -> id, textMap.[id])
        |> Seq.tryPick (fun (id, text) ->  transformToMatch getBottomEdge (getTopEdge tile.Text) text |> Option.map (fun newText -> id, newText))

    let (tileListTop, newTextMap, newUnassignedTileIds) = moveVerticallyFillLeft (List.singleton firstRow) textMap unassignedTileIds getTopTile
    textMap <- newTextMap
    unassignedTileIds <- newUnassignedTileIds

    // step 3: the same as step 2 for lines below
    let getBottomTile (tile: Tile) (textMap: Map<int, string list>) (unassignedIds: Set<int>) =
        unassignedIds
        |> Seq.map (fun id -> id, textMap.[id])
        |> Seq.tryPick (fun (id, text) ->  transformToMatch getTopEdge (getBottomEdge tile.Text) text |> Option.map (fun newText -> id, newText))

    let (tileList, _, newUnassignedTileIds) = moveVerticallyFillLeft (List.rev tileListTop) textMap unassignedTileIds getBottomTile
    if not newUnassignedTileIds.IsEmpty then
        printfn "warning: not all tiles assigned. Unassigned tiles: %s" (String.Join(", ", newUnassignedTileIds))

    tileList
    |> List.map (List.map (fun tile -> tile.Id, tile.Text))

// part 2
let extractPicture (tiles: string list list list) =
    let lines = 
        [
            for tileRow in tiles do
                for y in 1 .. tileRow.[0].Length - 2 do
                    yield
                        [
                            for tile in tileRow do
                                yield tile.[y].Substring(1, tile.[y].Length - 2)
                        ]
        ]
    lines
    |> List.map (List.reduce (+))

let allMatchIndices (regexStr: string) (search: string) =
    let regex = Regex regexStr

    let rec recAllMatchIndices list idx =
        let mat = regex.Match(search, idx)
        if mat.Success then
            recAllMatchIndices (mat.Index::list) (mat.Index + 1)
        else
            List.rev list

    recAllMatchIndices List.empty 0

let findSeaMonsterEndLineOccurances str =
    allMatchIndices ".#..#..#..#..#..#..." str

let confirmSeamonsterAt (picture: string list) (bottomLineIdx, charIdx) =
    bottomLineIdx > 1
    && picture.[bottomLineIdx - 2].[charIdx + 18] = '#'
    && allMatchIndices  "#....##....##....###" picture.[bottomLineIdx - 1] |> Set.ofSeq |> Set.contains charIdx

let getNrOfSeaMonsters (picture: string list) =
    let getNrOfSeaMonstersInSpecificPicture (specificPic: string list) =
        specificPic
        |> Seq.ofList
        |> Seq.indexed
        |> Seq.skip 2
        |> Seq.collect (fun (lineIdx, text) ->
            findSeaMonsterEndLineOccurances text
            |> Seq.map (fun charIdx -> lineIdx, charIdx)
            )
        |> Seq.filter (confirmSeamonsterAt specificPic)
        |> Seq.length

    getAllVariationsOfText picture
    |> Seq.map getNrOfSeaMonstersInSpecificPicture
    |> Seq.max

// bonus
let markSeaMonsters (picture: string list) =
    let putSeamonster (pic: Map<int, string>) ((lineIdx, charIdx): int * int) =
        let replaceCharsAt (str: string) (charPos: Set<int>) =
            str
            |> String.mapi (fun i ch -> if charPos.Contains i then 'O' else ch)
        
        pic
            .Add(lineIdx - 2, replaceCharsAt pic.[lineIdx - 2] (Set.singleton (charIdx + 18)))
            .Add(lineIdx - 1, replaceCharsAt pic.[lineIdx - 1] ([0; 5; 6; 11; 12; 17; 18; 19] |> List.map ((+) charIdx) |> Set.ofList))
            .Add(lineIdx, replaceCharsAt pic.[lineIdx] ([1; 4; 7; 10; 13; 16] |> List.map ((+) charIdx) |> Set.ofList))

    let markSeaMonstersInSpecificPicture (specificPic: string list) =
        let smIndices =
            specificPic
            |> Seq.ofList
            |> Seq.indexed
            |> Seq.skip 2
            |> Seq.collect (fun (lineIdx, text) ->
                findSeaMonsterEndLineOccurances text
                |> Seq.map (fun charIdx -> lineIdx, charIdx)
                )
            |> Seq.filter (confirmSeamonsterAt specificPic)
            |> List.ofSeq
        if smIndices.Length = 0 then
            None
        else
            Some (
                smIndices
                |> List.fold putSeamonster (specificPic |> Map.ofIndexSeq)
                |> Map.toList
                |> List.sortBy fst
                |> List.map snd
            )

    getAllVariationsOfText picture
    |> Seq.pick markSeaMonstersInSpecificPicture

let getNrOfBlackPixels (picture: string list) =
    picture
    |> Seq.collect id
    |> Seq.filter ((=) '#')
    |> Seq.length

[<EntryPoint>]
let main argv =
    // part 1
    let assembledTiles =
        getData argv
        |> assembleTiles

    let tl = assembledTiles |> List.head |> List.head |> fst |> int64
    let tr = assembledTiles |> List.head |> List.last |> fst |> int64
    let bl = assembledTiles |> List.last |> List.head |> fst |> int64
    let br = assembledTiles |> List.last |> List.last |> fst |> int64

    printfn "%d" (tl * tr * bl * br)

    // part 2
    let picture =
        assembledTiles
        |> List.map (List.map snd)
        |> extractPicture
 
    let BLACK_PIXELS_PER_SEAMONSTER = 15

    let nrOfSM = getNrOfSeaMonsters picture
    let nrOfBlackPixels = getNrOfBlackPixels picture

    printfn "%d" (nrOfBlackPixels - (nrOfSM * BLACK_PIXELS_PER_SEAMONSTER))

    // for if you want to print them (replaces sea monsters with 'O')
    //let printTile text =
    //    for line in text do
    //        printfn "%s" line
    //    printfn ""

    //printTile (markSeaMonsters picture)
    //printfn "%d" (picture |> markSeaMonsters |> getNrOfBlackPixels)

    0
