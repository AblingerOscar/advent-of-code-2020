open Lib
open System

type Movement =
| East
| SouthEast
| SouthWest
| West
| NorthEast
| NorthWest

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

[<EntryPoint>]
let main argv =
    getData argv
    |> Seq.map transformToCoordinates
    |> Seq.countBy id
    |> Seq.filter (fun (_, cnt) -> cnt % 2 <> 0)
    |> Seq.map fst
    |> Seq.length
    |> printfn "%d"

    0
