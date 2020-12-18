module Lib

open System
open System.Text.RegularExpressions

// *********** Extensions ***********

module Seq =
    let index source = Seq.mapi (fun i c -> (i, c)) source
    let mapt f (source: seq<'a * 'b>) = Seq.map (fun (a, b) -> f a b) source

    let triplewise (source: seq<_>) = seq {
        use e = source.GetEnumerator()
        if e.MoveNext () then
            let a = ref e.Current
            if e.MoveNext () then
                let b = ref e.Current
                while e.MoveNext () do
                    let c = e.Current
                    yield (!a, !b, c)
                    a := !b
                    b := c
    }

    /// Splits a sequence into subsequences at all elements that the given predicate returns true for
    /// The triggering element is included in the following subsequences
    let splitBy f input =
        let i = ref 0
        input
        |> Seq.groupBy (fun s -> (if f s then incr i) ; !i)
        |> Seq.map snd

    /// Splits a sequence into subsequence at all elements that the given predicate returns true for
    /// The triggering element is not included in any subsequence
    let splitWhen f input =
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

module List =
    /// Splits a list into sublists at all elements that the given predicate returns true for
    /// The triggering element is included in the following sublist
    let splitBy f input =
        let i = ref 0
        input
        |> List.groupBy (fun s -> (if f s then incr i) ; !i)
        |> List.map snd
    
    /// Splits a list into sublists at all elements that the given predicate returns true for
    /// The triggering element is not included in any sublist
    let splitWhen f input =
        let i = ref 0
        input
        |> List.choose (fun s ->
                        if f s then
                            incr i
                            None
                        else
                            Some (!i, s))
        |> List.groupBy fst
        |> List.map (snd >> List.map snd)

module Map =
    let ofIndexSeq<'t> : seq<'t> -> Map<int, 't> =
        Seq.index >> Map.ofSeq


// *********** Auxiliary methods ***********
let opt<'t> (tup: bool * 't) =
    match tup with
    | true, v -> Some v
    | false, _ -> None

let readEmptyLineSeparatedSections fileName =
    System.IO.File.ReadLines fileName
    |> Seq.splitWhen ((=) "")

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
