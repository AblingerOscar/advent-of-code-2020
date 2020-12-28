open Lib
open System
open System.Collections.Generic

let getData argv =
    System.IO.File.ReadLines (getFileName argv)
    |> Seq.map (Int32.Parse)

module Loop =
    let maxNumber = 20_201_227L

    let loopOnce v subjectNumber = int32 ((int64 v * int64 subjectNumber) % maxNumber)
    let rec loopNTimes n v subjectNumber =
        if n <= 0 then
            v
        else
            loopNTimes (n-1) (loopOnce v subjectNumber) subjectNumber

    // loop size
    let private loopSizeCache = Dictionary<int, int>(dict (seq { 1, 0 }))
    let publicKeySubjectNumber = 7
    let mutable private maxLoopVal = 1

    let rec fillLoopSizeCacheUntil (n: int) =
        let value = loopOnce maxLoopVal publicKeySubjectNumber
        loopSizeCache.Add(value, loopSizeCache.[maxLoopVal] + 1)
        maxLoopVal <- value

        if value <> n then
            fillLoopSizeCacheUntil n

    let calculateLoopSize (n: int) =
        if loopSizeCache.ContainsKey n then
            loopSizeCache.[n]
        else
            fillLoopSizeCacheUntil n
            loopSizeCache.[n]

[<EntryPoint>]
let main argv =
    let publicKeys = getData argv
    let pk1 = Seq.head publicKeys
    let pk2 = Seq.head (Seq.tail publicKeys)

    let ls1 = Loop.calculateLoopSize pk1
    let ls2 = Loop.calculateLoopSize pk2

    printfn "%d" (Loop.loopNTimes ls1 1 pk2)
    0
