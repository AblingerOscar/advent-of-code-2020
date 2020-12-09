open Lib
open System

[<Literal>]
let LOOK_BEHIND_LENGTH = 25


let getData argv =
    System.IO.File.ReadLines (getFileName argv)
    |> Seq.map (Int32.TryParse >> opt)
    |> Seq.choose id

let isValid (arr: int[]) =
    let nr = Array.last arr
    let prev = Set.ofArray arr.[0..LOOK_BEHIND_LENGTH]

    prev
    |> Seq.exists (fun x -> prev.Contains (nr - x))


let calcEncryptionWeaknessSeq invalidNumber (list: 't list) =
    let rec calcRec startIdx endIdx =
        let slice = list.GetSlice (Some startIdx, Some endIdx)
        match (Seq.sum slice, slice) with
        | sum, slice when sum = invalidNumber -> slice
        | sum, _ when sum > invalidNumber -> calcRec (startIdx+1) endIdx
        | sum, _ when sum < invalidNumber -> calcRec startIdx (endIdx+1)
        | _, _ -> failwith "error"

    calcRec 0 0

[<EntryPoint>]
let main argv =
    let data = getData argv

    let firstInvalidNr =
        data
        |> Seq.windowed (LOOK_BEHIND_LENGTH + 1)
        |> Seq.find (isValid >> not)
        |> Array.last
    
    printfn "%d" firstInvalidNr

    data
    |> List.ofSeq
    |> calcEncryptionWeaknessSeq firstInvalidNr
    |> (fun s -> (Seq.min s) + (Seq.max s))
    |> printfn "%d"

    0
