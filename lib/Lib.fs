module Lib

open System

let splitBy f input =
    let i = ref 0
    input
    |> Seq.groupBy (fun s -> (if f s then incr i) ; !i)
    |> Seq.map snd

let splitAt f input =
    let i = ref 0
    input
    |> Seq.choose (fun s ->
                    if f s then
                        incr i
                        None
                    else
                        Some (!i, s))
    |> Seq.groupBy fst
    |> Seq.map (snd >> Seq.map snd)

let readEmptyLineSeparatedSections fileName =
    System.IO.File.ReadLines fileName
    |> splitAt ((=) "")
    

let waitForUser () =
    printfn "Finished. Press 'e' to exit the program…"

    let rec doUntil (f: unit -> ConsoleKeyInfo) expected =
        if f().KeyChar <> expected then
            doUntil f expected

    doUntil System.Console.ReadKey 'e'