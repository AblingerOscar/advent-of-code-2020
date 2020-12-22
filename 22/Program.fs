open Lib
open System
open FSharpx.Collections

let getData argv =
    readEmptyLineSeparatedSections (getFileName argv)
    |> Seq.map (Seq.skip 1)
    |> Seq.map (Seq.map Int32.Parse)
    |> (fun sequences -> (Seq.head sequences, Seq.head (Seq.tail sequences)))

let playGameUntilEnd ((player1, player2): seq<int> * seq<int>): list<int> * list<int> =
    // nice functional code
    let rec play (q1: Queue<int>) (q2: Queue<int>): (Queue<int> * Queue<int>) =
        if q1.IsEmpty || q2.IsEmpty then
            (q1, q2)
        else
            match q1.Uncons, q2.Uncons with
            | (h1,t1), (h2, t2) when h1 > h2 -> play (t1.Conj(h1).Conj(h2)) t2
            | (h1,t1), (h2, t2) when h1 < h2 -> play t1 (t2.Conj(h2).Conj(h1))
            | _ -> failwithf "apparently two cards have the same value"

    play (Queue.ofSeq player1) (Queue.ofSeq player2)
    |> (fun (q1, q2) -> (List.ofSeq q1, List.ofSeq q2))

[<EntryPoint>]
let main argv =
    getData argv
    |> playGameUntilEnd
    ||> fun l r ->
        match l.Length, r.Length with
        | a, 0 -> l
        | 0, a -> r
        | _ -> failwith "apparently the game stopped before it was finished"
    |> List.rev
    |> List.indexed
    |> List.map (fun (i, v) -> (i+1) * v)
    |> List.reduce (+)
    |> printfn "%d"

    0
