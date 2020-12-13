open Lib
open System

type Bus =
    | X
    | Id of int

let parseBus str =
    match str with
    | "x" -> X
    | x -> Id (Int32.Parse x)

let getData argv =
    let text = System.IO.File.ReadLines (getFileName argv)
                |> List.ofSeq
    let buses =
        text.[1].Split ','
        |> Array.map parseBus
    (Int32.Parse text.[0]), buses

let getData2 argv =
    System.IO.File.ReadLines (getFileName argv)
    |> Seq.skip 1
    |> Seq.head
    |> (fun s -> s.Split ',')
    |> Array.map parseBus

let part1 argv =
    let (start, buses) = getData argv
    buses
    |> Seq.map (fun bus ->
        match bus with
        | X -> None
        | Id id -> Some (id, id - (start % id)))
    |> Seq.choose id
    |> Seq.sortBy snd
    |> Seq.head
    ||> (*)
    |> printfn "%d"

let getLargest buses =
    buses
    |> Seq.sortByDescending snd
    |> Seq.head

let part2 argv =
    let buses = getData2 argv
    let interestingBuses =
        buses
        |> Seq.indexed
        |> Seq.map (fun (offset, bus) ->
            match bus with
            | X -> None
            | Id id -> Some (int64 offset, int64 id))
        |> Seq.choose id

    let isValid (timestamp: int64) =
        interestingBuses
        |> Seq.forall (fun (offset, id) -> ((timestamp + offset) % id = 0L))

    let (largestBusOffset, largestBusId) = getLargest interestingBuses

    // try all multiples of the largest bus - its offset (aka the starting timestamp)
    Seq.initInfinite (fun i -> largestBusId * (int64 i) - int64 largestBusOffset)
    |> Seq.find isValid
    |> printfn "%d"

[<EntryPoint>]
let main argv =
    part1 argv
    part2 argv

    0
