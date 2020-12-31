open ActivePatterns
open Lib
open System

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

let getTopEdge text = List.head text
let getBottomEdge text = List.last text
let getLeftEdge (text: string list) = text |> List.map (fun line -> line.[0]) |> String.Concat
let getRightEdge (text: string list) = text |> List.map (fun line -> line.[line.Length - 1]) |> String.Concat

// pretty inefficient, but it works
let transformToMatch (getEdge: string list -> string) (edge: string) (text: string list) =
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

let printTile text =
    for line in text do
        printfn "%s" line
    printfn ""

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

    0
