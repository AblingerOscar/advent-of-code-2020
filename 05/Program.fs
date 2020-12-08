open Lib

let getData argv = seq {
    yield! System.IO.File.ReadLines (getFileName argv)
}

let getSeat seatDescriptor =
    let rec recGetSeat descriptor rowMin rowMax columnMin columnMax =
        let middle min max = (min + max + 1) / 2
        match descriptor with
        | 'B'::t -> recGetSeat t (middle rowMin rowMax) rowMax columnMin columnMax
        | 'F'::t -> recGetSeat t rowMin (middle rowMin rowMax) columnMin columnMax
        | 'R'::t -> recGetSeat t rowMin rowMax (middle columnMin columnMax) columnMax
        | 'L'::t -> recGetSeat t rowMin rowMax columnMin (middle columnMin columnMax)
        | _ -> rowMin, columnMin

    recGetSeat seatDescriptor 0 127 0 7

let calcSeatId (r, c) = r * 8 + c

[<EntryPoint>]
let main argv =
    let seatIds =
        getData argv
        |> Seq.map (Seq.toList >> getSeat >> calcSeatId)
        |> Seq.toList
        |> List.sort

    let maxId = List.last seatIds
    printfn "The highest seat id is %d" maxId

    let myId =
        List.pairwise seatIds
        |> List.find (fun (a, b) -> a + 1 <> b)
        |> snd
        |> fun a -> a - 1

    printfn "My seat id is %d" myId

    Lib.waitForUser ()
    0 // return an integer exit code
