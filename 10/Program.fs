open Lib
open System
open System.Collections.Generic

let getData argv =
    System.IO.File.ReadLines (getFileName argv)
    |> Seq.map Int32.Parse


let cache = Dictionary<int list, int64>()
cache.Add([1; 1], 2L)
cache.Add([2; 1], 2L)
cache.Add([1; 2], 2L)

let rec calc3Perm (source: int list) =
    if source.Length <= 1 then
        1L
    // 1) split at occurances of 3 or [2; 2]
    else if List.last source = 3 then
        calc3Perm source.[.. source.Length-2]
    else
        match List.tryFindIndex ((=) 3) source with
        | Some i -> (calc3Perm source.[.. i-1]) * (calc3Perm source.[i+1..])
        | None ->
            match List.tryFindIndex ((=) (2, 2)) (List.pairwise source) with
            | Some i -> (calc3Perm source.[..i-1]) * (calc3Perm source.[i+2..])
            | None ->

                //2) unsplittable list
                match cache.TryGetValue source with
                | (true, v) -> v
                | (false, _) ->
                    // destructuring is okay, because we know that the length here has to be 2 or more
                    let a::b::t = source
                    let result = (calc3Perm (b::t)) + (calc3Perm ((a+b)::t))
                    cache.Add(source, result)
                    result

[<EntryPoint>]
let main argv =
    // part 1
    getData argv
    |> Seq.append (seq { 0 })
    |> Seq.sort
    |> Seq.pairwise
    |> Seq.fold (fun (ones, threes) (a, b) ->
        match b - a with
        | 1 -> (ones+1, threes)
        | 3 -> (ones, threes+1)
        | _ -> (ones, threes)
    ) (0, 1) // one for the built-in adapter of the device
    ||> (*)
    |> printfn "%d"


    // part 2
    let rs =
        getData argv
        |> Seq.append (seq { 0 })
        |> Seq.sort
        |> Seq.pairwise
        |> Seq.mapt (-)
        |> Seq.map abs
        |> Seq.toList
        |> calc3Perm
    printfn "%d" rs
    printfn "iscorrect=%b" (rs = 43_406_276_662_336L)

    0
