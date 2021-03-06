﻿open Lib
open System

// part 1
let getData argv = seq {
    let lines = System.IO.File.ReadLines (getFileName argv)

    let mutable result = ""
    
    for line in lines do
        if String.IsNullOrEmpty line then
            yield result
            result <- ""
        else
            result <- result + line

    if not (String.IsNullOrEmpty result) then
        yield result
}

let getCntOfDiffAnswers answers = (Set.ofSeq answers).Count

// part 2
let getData2 argv = seq {
    yield! readEmptyLineSeparatedSections (getFileName argv)
}

let getCntOfSameAnswers answers =
    Seq.map Set.ofSeq answers
    |> Set.intersectMany
    |> Set.count

[<EntryPoint>]
let main argv =
    // part 1
    getData argv
    |> Seq.map getCntOfDiffAnswers
    |> Seq.reduce (+)
    |> printfn "Sum of counts (any yes): %d"

    // part 2
    getData2 argv
    |> Seq.map getCntOfSameAnswers
    |> Seq.reduce (+)
    |> printfn "Sum of counts (all yes): %d"

    0 // return an integer exit code
