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
    let start = 100000000000000L - (100000000000000L % largestBusId)
    Seq.initInfinite (fun i -> start + largestBusId * (int64 i) - int64 largestBusOffset)
    |> Seq.find isValid
    |> printfn "%d"


let rec chineseRemainder (cs: (int64 * int64) list) (x: int64) (N: int64) =
    match cs with
    | [] -> Some(x)
    | (a,n)::rest ->
        let arrProgress = Seq.unfold (fun x -> Some(x, x+N)) x
        let firstXmodNequalA = Seq.tryFind (fun x -> a = x % n)
        match firstXmodNequalA (Seq.take (int n) arrProgress) with
        | None -> None
        | Some(x) -> chineseRemainder rest x (N*n)

let part2b argv =
    let rec posMod (n: int64) (m: int64) =
        let r = (n % m)
        if r < 0L then
            posMod (n + m) m
        else
            r

    let interestingBuses =
        getData2 argv
        |> Seq.indexed
        |> Seq.map (fun (offset, bus) ->
            match bus with
            | X -> None
            | Id id -> Some (int64 offset, int64 id))
        |> Seq.choose id
        |> Seq.map (fun (offset, id) -> (posMod (-offset) id), id)
        |> List.ofSeq


    let cs =
        interestingBuses
        |> List.map (fun (offset, id) -> (offset % id, id))
        |> List.sortByDescending snd

    match cs with
    | (x, N)::t -> 
        match chineseRemainder t x N with
        | None -> printfn "invalid -> no solution"
        | Some x -> printfn "%d" x
    | _ -> printfn "empty list"


[<EntryPoint>]
let main argv =
    part1 argv
    part2b argv

    0
