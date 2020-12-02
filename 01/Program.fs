open System
open Lib

let firstPart (numbers: list<int>) =
    let rec allPairs l = seq {
        match l with
        | [] -> yield! Seq.empty
        | (x::xs) ->
            for y in xs do
                yield (x, y)
            yield! allPairs xs
    }

    printfn "number of numbers: %d" numbers.Length

    let n = ref 0

    for (x, y) in allPairs numbers do
        incr n
        if x + y = 2020 then
            printfn "Found combination: %d and %d sum up to 2020 and their product is %d" x y (x*y)
    
    printfn "checked %d pairs" !n

let secondPart numbers =
    let rec allPairs l = seq {
        match l with
        | [] -> yield! Seq.empty
        | (x::xs) ->
            for y in xs do
                yield (x, y)
            yield! allPairs xs
    }

    let rec allTriples l = seq {
        match l with
        | [] -> yield! Seq.empty
        | (x::xs) ->
            for (y, z) in allPairs xs do
                yield (x, y, z)
            yield! allTriples xs
    }

    let n = ref 0
    
    for (x, y, z) in allTriples numbers do
        incr n
        if x + y + z = 2020 then
            printfn "Found combination: %d, %d and %d sum up to 2020 and their product is %d" x y z (x*y*z)
    
    printfn "checked %d triples" !n



[<EntryPoint>]
let main argv =
    let numbers = System.IO.File.ReadLines "data/source.txt"
                    |> List.ofSeq
                    |> List.map int

    secondPart numbers

    waitForUser ()
    0 // return an integer exit code
