open Lib
open System

let getData argv =
    (System.IO.File.ReadAllText (getFileName argv)).Split ','
    |> Seq.map int
    
let nextNumber (map: Map<int, int>) (lastNr: int) (idx: int) =
    if map.ContainsKey lastNr then
        idx - map.[lastNr]
    else
        0

let calc1 ((lastNr, map): int * Map<int, int>) idx =
    let n = nextNumber map lastNr (idx-1)

    n, map.Add(lastNr, idx-1)

[<EntryPoint>]
let main argv =
    // part 1
    let startingInput = Seq.cache (getData argv)
    let lastNumber = Seq.last startingInput
    let startingMap =
        startingInput
        |> Seq.take (Seq.length startingInput)
        |> Seq.indexed
        |> Seq.map (fun (a, b) -> b,a)
        |> Map.ofSeq

    seq { startingMap.Count .. 2019 }
    |> Seq.fold calc1 (lastNumber, startingMap)
    |> fst
    |> printfn "%d"

    seq { startingMap.Count .. 30000000 - 1 }
    |> Seq.fold calc1 (lastNumber, startingMap)
    |> fst
    |> printfn "%d"

    0 // return an integer exit code
