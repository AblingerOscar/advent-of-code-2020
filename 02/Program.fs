// Learn more about F# at http://fsharp.org

open System
open System.Runtime.CompilerServices
open System.Text.RegularExpressions
open Lib

[<IsReadOnly; Struct>]
type Password(min: int, max: int, letter: char, input: string) =
    member x.IsValidV1 =
        let count = Seq.filter ((=) letter) input
                    |> Seq.length
        min <= count && count <= max
    member x.IsValidV2 =
        (input.[min - 1] = letter) <> (input.[max - 1] = letter)

let parsePassword group =
    try
        match group with
        | [_; min; max; letter; input] -> Some (Password(
                                                    Int32.Parse min,
                                                    Int32.Parse  max,
                                                    Seq.map char letter |> Seq.head,
                                                    input))
        | _ -> None
    with
        _ -> None

let getData () = seq {
    let lines = System.IO.File.ReadLines "data/source.txt"

    for line in lines do
        let m = Regex.Match(line, "(\d+)-(\d+) ([a-z]): (.*)")
        if m.Success then
            let po = m.Groups.Values
                        |> Seq.map (fun g -> g.Value)
                        |> List.ofSeq
                        |> parsePassword

            if po.IsSome then
                yield po.Value
}

[<EntryPoint>]
let main argv =

    let mutable cnt = 0

    for password in getData() do
        if password.IsValidV2 then
            cnt <- cnt + 1
    
    printfn "Found %d valid passwords" cnt
    waitForUser()
    0 // return an integer exit code
