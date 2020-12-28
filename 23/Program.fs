open Lib
open System

let getData argv =
    let charToInt c = int c - int '0'

    System.IO.File.ReadLines (getFileName argv)
    |> Seq.head
    |> Seq.map charToInt
    |> List.ofSeq

let pickNextThree (list: int list) idx =
    let selectedIdx =
        seq {
            for i in idx + 1 .. idx + 3 do
                yield i % list.Length
        }

    let selectedIdxSet =
        selectedIdx
        |> Set.ofSeq

    let newList =
        list
        |> List.indexed
        |> List.filter (fst >> selectedIdxSet.Contains >> not)
        |> List.map snd

    let selected =
        selectedIdx
        |> Seq.map (fun i -> list.[i])
        |> List.ofSeq

    (newList, selected)

let rec getDestinationCupIdx list label =
    if label - 1 < 0 then
        list
        |> List.indexed
        |> List.maxBy snd
        |> fst
    else
        match List.tryFindIndex ((=) (label - 1)) list with
        | Some x -> x
        | None -> getDestinationCupIdx list (label - 1)

let playOneRound list currIdx =
    let (newList, selected) = pickNextThree list currIdx
    let destination = getDestinationCupIdx newList list.[currIdx]
    
    if destination = newList.Length - 1 then
        newList @ selected
    else
        newList.[..destination] @ selected @ newList.[destination + 1..]

// only for debugging
let printlist list =
    for i in list do
        printf "%d" i
    printfn ""

let rec playNTimes startIdx times (list: int list) =
    let getNextId newList =
        newList
        |> List.findIndex ((=) list.[startIdx])
        |> fun i -> (i + 1) % newList.Length

    if times = 1 then
        let newList = playOneRound list startIdx
        (getNextId newList, newList)
    else
        let newList = playOneRound list startIdx
        
        playNTimes (getNextId newList) (times-1) newList

let printListStartingFrom1 (list: int list) =
    let idx1 = List.findIndex ((=)1) list

    for i in idx1 + 1 .. list.Length - 1 do
        printf "%d" list.[i]

    for i in 0 .. idx1 - 1 do
        printf "%d" list.[i]

    printfn ""

[<EntryPoint>]
let main argv =
    let data = getData argv

    // part 1
    data
    |> playNTimes 0 100
    |> snd
    |> printListStartingFrom1

    // part 2
    let (idx, list) =
        data @ [ 10 .. 1_000_000 ]
        |> playNTimes 0 10_000_000

    let a = list.[idx]
    let b = list.[(idx + 1) % list.Length]

    printfn "%d * %d = %d" a b (a*b)

    0
