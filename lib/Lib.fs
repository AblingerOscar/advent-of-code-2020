﻿module Lib

open System
open System.Text.RegularExpressions

let opt<'t> (tup: bool * 't) =
    match tup with
    | true, v -> Some v
    | false, _ -> None

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

let regexMap regex transform source =
    let groupMap = Regex(regex).Match(source).Groups
                   |> Seq.skip 1
                   |> Seq.filter (fun group -> not (Seq.forall Char.IsDigit group.Name))
                   |> Seq.map (fun group -> group.Name, group.Value)
                   |> Map.ofSeq
    transform groupMap

let getFileName (argv: string array) =
    if argv.Length <> 0 then
        argv.[0]
    else
        "data/source.txt"

let waitForUser () =
    printfn "Finished. Press 'e' to exit the program…"

    let rec doUntil (f: unit -> ConsoleKeyInfo) expected =
        if f().KeyChar <> expected then
            doUntil f expected

    doUntil System.Console.ReadKey 'e'


module Seq =
    let index source = Seq.mapi (fun i c -> (i, c)) source

module Map =
    let ofIndexSeq<'t> : seq<'t> -> Map<int, 't> =
        Seq.index >> Map.ofSeq