open Lib
open System

[<Literal>]
let DO_PRINT_MAP = false

type Seat =
    | Empty
    | Occupied
    | Floor

let printMap map =
    if DO_PRINT_MAP then
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

let isOOB x y (map: 't list list) = x < 0 || x >= map.Length || y < 0 || y >= map.[x].Length

let getNrOfAdjacentOccupiedSeats x y (map: Seat list list) =
    let s = List.ofSeq (seq {
        for i in [-1..1] do
            for j in [-1..1] do
                if not (i = 0 && j = 0) then
                    if not (isOOB (x+i) (y+j) map) then
                        yield x+i, y+j, map.[x+i].[y+j]
    })
    s
    |> Seq.map (fun (_,_,v) -> v)
    |> Seq.filter ((=) Occupied)
    |> Seq.length

let getNrOfSeenOccupiedSeats x y (map: Seat list list) =
    let rec occupiedSeatInView ((sx, sy): int*int) ((ix, iy): int*int) =
        if isOOB (sx + ix) (sy + iy) map then
            false
        else
            match map.[sx + ix].[sy + iy] with
            | Occupied -> true
            | Empty -> false
            | Floor -> occupiedSeatInView (sx+ix, sy+iy) (ix, iy)
    
    seq {
        for i in [-1..1] do
            for j in [-1..1] do
                if not (i = 0 && j = 0) then
                    yield occupiedSeatInView (x, y) (i, j)
    }
    |> Seq.filter id
    |> Seq.length

let runOnce (calcNrOfOccupiedNeighbors: int -> int -> Seat list list -> int) minOccupiedForMove (map: Seat list list) =
    let mutable changeHappened = false
    let newMap = [
        for x, inner in Seq.indexed map do
            yield [
                for y, v in Seq.indexed inner do
                    let nr = calcNrOfOccupiedNeighbors x y map
                    yield match v, nr with
                            | Empty, 0 ->
                                changeHappened <- true
                                Occupied
                            | Occupied, x when x >= minOccupiedForMove ->
                                changeHappened <- true
                                Empty
                            | _ -> v
            ]
    ]
    changeHappened, newMap

let runUntilStable (calcNrOfOccupiedNeighbors: int -> int -> Seat list list -> int) minOccupiedForMove map =
    let rec recRunUntilStable (changeHappened, newMap) =
        if changeHappened then
            printMap newMap
            recRunUntilStable (runOnce calcNrOfOccupiedNeighbors minOccupiedForMove newMap)
        else
            newMap
    
    recRunUntilStable (runOnce calcNrOfOccupiedNeighbors minOccupiedForMove map)

[<EntryPoint>]
let main argv =
    // part 1
    getData argv
    |> runUntilStable getNrOfAdjacentOccupiedSeats 4
    |> Seq.collect id
    |> Seq.filter ((=) Occupied)
    |> Seq.length
    |> printfn "%d"

    // part 2
    getData argv
    |> runUntilStable getNrOfSeenOccupiedSeats 5
    |> Seq.collect id
    |> Seq.filter ((=) Occupied)
    |> Seq.length
    |> printfn "%d"
    0
