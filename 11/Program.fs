open Lib
open System

type Seat =
    | Empty
    | Occupied
    | Floor

let printMap map =
    for _, inner in Seq.indexed map do
        for _, v in Seq.indexed inner do
            printf (match v with
                    | Empty -> "L"
                    | Occupied -> "#"
                    | Floor -> ".")
        printfn ""
    printfn "" // just for empty line

let getData argv =
    System.IO.File.ReadLines (getFileName argv)
    |> List.ofSeq
    |> List.map (fun l -> l |> Seq.map (fun c -> 
        match c with
        | '.' -> Floor
        | 'L' -> Empty
        | '#' -> Occupied
        | _ -> failwith "invalid input"
    ) >> List.ofSeq)

let getNrOfAdjacentOccupiedSeats x y (map: Seat list list) =
    let s = List.ofSeq (seq {
        for i in [-1..1] do
            for j in [-1..1] do
                if not (i = 0 && j = 0) then
                    if x + i >= 0 && x + i < map.Length && y + j >= 0 && y + j < map.[x + i].Length then
                        yield x+i, y+j, map.[x+i].[y+j]
    })
    s
    |> Seq.map (fun (_,_,v) -> v)
    |> Seq.filter ((=) Occupied)
    |> Seq.length

let runOnce (map: Seat list list) =
    let mutable changeHappened = false
    let newMap = [
        for x, inner in Seq.indexed map do
            yield [
                for y, v in Seq.indexed inner do
                    let nr = getNrOfAdjacentOccupiedSeats x y map
                    yield match v, nr with
                            | Empty, 0 ->
                                changeHappened <- true
                                Occupied
                            | Occupied, x when x >= 4 ->
                                changeHappened <- true
                                Empty
                            | _ -> v
            ]
    ]
    changeHappened, newMap

let runUntilStable map =
    let rec recRunUntilStable (changeHappened, newMap) =
        if changeHappened then
            recRunUntilStable (runOnce newMap)
        else
            newMap
    
    recRunUntilStable (runOnce map)

[<EntryPoint>]
let main argv =
    let numberOccupied =
        getData argv
        |> runUntilStable
        |> Seq.collect id
        |> Seq.filter ((=) Occupied)
        |> Seq.length

    printfn "%d" numberOccupied

    0
