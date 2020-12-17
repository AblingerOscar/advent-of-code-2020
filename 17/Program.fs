open Lib
open System
open System.Runtime.CompilerServices

// x,y,z tuples of active cubes
type Tensor = Set<int list>

let getData argv: Tensor =
    System.IO.File.ReadLines (getFileName argv)
    |> Seq.map Seq.index
    |> Seq.map (Seq.filter (fun (_, str) -> str = '#'))
    |> Seq.map (Seq.map fst)
    |> Seq.index
    |> Seq.collect (fun (y, xes) -> Seq.map (fun x -> [x; y; 0]) xes)
    |> Set.ofSeq

let threeDimRange s e = seq {
    for i in s .. e do
        for j in s .. e do
            for k in s .. e do
                yield i, j, k
}

let allNeighborsAndSelf3D list = seq {
    match list with
    | x::y::z::t when t.Length = 0 ->
        for i, j, k in threeDimRange -1 1 do
            yield [x+i; y+j; z+k]
    | _ -> failwithf "false amount of elements in list (expected: 3, got: %d)" list.Length
}

let doOneCycle (neighborAndSelfFunction: 'a -> 'a seq) (tensor: Set<'a>): Set<'a> =
    let t =
        tensor
        |> Seq.collect neighborAndSelfFunction
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
            neighborAndSelfFunction pos
            |> Seq.filter tensor.Contains
            |> Seq.length
            |> fun l -> l = 3 || l = 4 // 3 or 4 instead of 2 or 3, because the active element itself is also included
        )
        |> Set.ofSeq

    inactiveToActive + stayActive

[<EntryPoint>]
let main argv =
    let rec repeat n f =
        if n = 0 then
            f
        else
            f >> (repeat (n-1) f)

    getData argv
    |> repeat 5 (doOneCycle allNeighborsAndSelf3D)
    |> Seq.length
    |> printfn "%d"

    0
