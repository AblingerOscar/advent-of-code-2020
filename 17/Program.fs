open Lib
open System
open System.Runtime.CompilerServices

// x,y,z tuples of active cubes
type Tensor = Set<int * int * int>

let getData argv =
    System.IO.File.ReadLines (getFileName argv)
    |> Seq.map Seq.index
    |> Seq.map (Seq.filter (fun (_, str) -> str = '#'))
    |> Seq.map (Seq.map fst)
    |> Seq.index
    |> Seq.collect (fun (y, xes) -> Seq.map (fun x -> (x, y, 0)) xes)
    |> Set.ofSeq

let threeDimRange s e = seq {
    for i in s .. e do
        for j in s .. e do
            for k in s .. e do
                yield i, j, k
}

let allNeighborsAndSelf (x, y, z) = seq {
    for i, j, k in threeDimRange -1 1 do
        yield x+i, y+j, z+k
}

let doOneCycle (tensor: Tensor): Tensor =
    let t =
        tensor
        |> Seq.collect allNeighborsAndSelf
        |> Seq.filter (tensor.Contains >> not)
        // the times it is in the sequence is amount of active neighbors
        |> Seq.countBy id

    let inactiveToActive =
        t
        |> Seq.filter (fun (_, cnt) -> cnt = 3)
        |> Seq.map fst
        |> Set.ofSeq

    let stayActive =
        tensor
        |> Seq.filter (fun pos ->
            allNeighborsAndSelf pos
            |> Seq.filter tensor.Contains
            |> Seq.length
            |> fun l -> l = 3 || l = 4 // 3 or 4 instead of 2 or 3, because the active element itself is also included
        )
        |> Set.ofSeq

    inactiveToActive + stayActive

let print xes ys zeds (tensor: Tensor) =
    for z in zeds do
        printfn "z=%d" z
        for y in ys do
            for x in xes do
                if tensor.Contains (x, y, z) then
                    printf "#"
                else
                    printf "."
            printfn ""
    ()

[<EntryPoint>]
let main argv =
    let rec repeat n f =
        if n = 0 then
            f
        else
            f >> (repeat (n-1) f)

    getData argv
    |> repeat 5 doOneCycle
    |> Seq.length
    |> printfn "%d"

    0
