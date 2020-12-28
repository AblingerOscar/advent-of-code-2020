open Lib
open System

type Movement =
| East
| SouthEast
| SouthWest
| West
| NorthEast
| NorthWest

// parses in reverse order, but this doesn't matter, since they will be normalized anyways
let parseLine str =
    let rec extract (result: Movement list) (chars: char list) =
        match chars with
        | [] -> result
        | 'e'::t -> extract (East::result) t
        | 's'::'e'::t -> extract (SouthEast::result) t
        | 's'::'w'::t -> extract (SouthWest::result) t
        | 'w'::t -> extract (West::result) t
        | 'n'::'e'::t -> extract (NorthEast::result) t
        | 'n'::'w'::t -> extract (NorthWest::result) t
        | _ -> failwithf "failed to parse '%s'" (String.Concat chars)
    extract List.Empty (List.ofSeq str)

let getData argv =
    System.IO.File.ReadLines (getFileName argv)
    |> Seq.map parseLine

let transformToCoordinates (tile: Movement list) =
    let rec calcCoordinates x y list =
        match list with
        | [] -> x, y
        | East::t -> calcCoordinates (x+1.0) y t
        | West::t -> calcCoordinates (x-1.0) y t
        | SouthEast::t -> calcCoordinates (x+0.5) (y-1.0) t
        | SouthWest::t -> calcCoordinates (x-0.5) (y-1.0) t
        | NorthEast::t -> calcCoordinates (x+0.5) (y+1.0) t
        | NorthWest::t -> calcCoordinates (x-0.5) (y+1.0) t

    calcCoordinates 0.0 0.0 tile

// part 2

type TileCoordinate = int * int
module TileCoordinate =
    let ofFloatCoordinate ((x, y): float * float) = (int (x * 2.0), int y)

let rec playGameOfLive numIter (activeTiles: Set<TileCoordinate>) =
    let getAdjacentTilesAndSelf (tx, ty as tile) = seq {
            yield tile             // self
            yield (tx + 2, ty)     // e
            yield (tx - 2, ty)     // w
            yield (tx + 1, ty + 1) // ne
            yield (tx - 1, ty + 1) // nw
            yield (tx + 1, ty - 1) // se
            yield (tx - 1, ty - 1) // sw
        }

    if numIter = 0 then
        activeTiles
    else
        let newActiveTiles =
            activeTiles
            |> Set.toSeq
            |> Seq.collect getAdjacentTilesAndSelf
            |> Seq.countBy id
            |> Seq.filter (fun (tile, cnt) ->
                // the count is the number of active neighbors & +1 if it is active itself
                match activeTiles.Contains tile, cnt with
                | true, n when n <= 3  && n <> 1 -> true
                | false, 2 -> true
                | _ -> false
            )
            |> Seq.map fst
            |> Set.ofSeq

        playGameOfLive (numIter - 1) newActiveTiles

[<EntryPoint>]
let main argv =
    let data =
        getData argv
        |> Seq.map transformToCoordinates

    // part 1
    let blackTiles =
        data
        |> Seq.countBy id
        |> Seq.filter (fun (_, cnt) -> cnt % 2 <> 0)
        |> Seq.map fst

    blackTiles
    |> Seq.length
    |> printfn "%d"

    // part 2
    blackTiles
    |> Seq.map TileCoordinate.ofFloatCoordinate
    |> Set.ofSeq
    |> playGameOfLive 100
    |> Set.count
    |> printfn "%d"

    0
