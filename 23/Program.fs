open Lib
open System
open System.Collections.Generic

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

// part 2 functions
module LinkedList =
    let value (node: LinkedListNode<int>) = node.Value
    let cyclingNext (list: LinkedList<int>) (node: LinkedListNode<int>) =
        match node.Next with
        | null -> list.First
        | next -> next

    let takeAfter (n: int) (node: LinkedListNode<int>) (list: LinkedList<int>) =
        [
            let mutable currNode = node
            for _ in 0..n do
                currNode <- cyclingNext list currNode
                yield currNode
        ]

    let moveManyAfter (afterNode: LinkedListNode<int>) (list: LinkedList<int>) (toAddNodes: LinkedListNode<int> list) =
        let mutable curr = afterNode
        for newNode in toAddNodes do
            list.Remove newNode
            list.AddAfter(curr, newNode)
            curr <- newNode

let playOneRoundLL (list: LinkedList<int>) (currNode: LinkedListNode<int>) =
    let removed =
        list
        |> LinkedList.takeAfter 3 currNode
    
    let removedVals =
        removed
        |> List.map (fun node -> node.Value)
        |> Set.ofList

    let mutable destinationVal = currNode.Value - 1
    if destinationVal = 0 then
        destinationVal <- 1_000_000
    while removedVals.Contains destinationVal do
        destinationVal <- destinationVal - 1
        if destinationVal = 0 then
            destinationVal <- 1_000_000

    let destination = list.Find destinationVal

    LinkedList.moveManyAfter destination list removed

let playNRoundsLL (n: int) (list: LinkedList<int>) (startNode: LinkedListNode<int>) =
    let mutable currNode = startNode
    for i in 0..n do
        playOneRoundLL list currNode
        currNode <- LinkedList.cyclingNext list currNode

        if i % 100_000 = 0 then
            printfn "update: %d%%" (i/100_000)
   
[<EntryPoint>]
let main argv =
    let data = getData argv

    // part 1
    data
    |> playNTimes 0 100
    |> snd
    |> printListStartingFrom1

    // part 2 – attempt 1: linked list
    let list = LinkedList<int> (data @ [ 10 .. 1_000_000 ])
    playNRoundsLL 10_000_000 list list.First

    LinkedList.takeAfter 2 (list.Find 1) list
    |> List.map LinkedList.value
    |> List.reduce (*)
    |> printfn "%d"

    0
