open System

let getData () = seq {
    let lines = System.IO.File.ReadLines "data/source.txt"

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

[<EntryPoint>]
let main argv =
    getData ()
    |> Seq.map getCntOfDiffAnswers
    |> Seq.reduce (+)
    |> printfn "Sum of counts: %d"

    0 // return an integer exit code
