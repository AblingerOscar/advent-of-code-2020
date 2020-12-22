open Lib
open System
open FSharpx.Collections

let getData argv =
    readEmptyLineSeparatedSections (getFileName argv)
    |> Seq.map (Seq.skip 1)
    |> Seq.map (Seq.map Int32.Parse)
    |> (fun sequences -> (Seq.head sequences, Seq.head (Seq.tail sequences)))

let playGameUntilEnd1 ((player1, player2): seq<int> * seq<int>): list<int> * list<int> =
    // nice functional code
    let rec play (q1: Queue<int>) (q2: Queue<int>): (Queue<int> * Queue<int>) =
        if q1.IsEmpty || q2.IsEmpty then
            (q1, q2)
        else
            match q1.Uncons, q2.Uncons with
            | (h1,t1), (h2,t2) when h1 > h2 -> play (t1.Conj(h1).Conj(h2)) t2
            | (h1,t1), (h2,t2) when h1 < h2 -> play t1 (t2.Conj(h2).Conj(h1))
            | _ -> failwithf "apparently two cards have the same value"

    play (Queue.ofSeq player1) (Queue.ofSeq player2)
    |> (fun (q1, q2) -> (List.ofSeq q1, List.ofSeq q2))

type Winner =
    | Player1 of int list
    | Player2 of int list

    member x.Value =
        match x with
        | Player1 v -> v
        | Player2 v -> v

    member x.WithValue value =
        match x with
        | Player1 _ -> Player1 value
        | Player2 _ -> Player2 value

let rec playRecursiveCombat ((player1, player2): seq<int> * seq<int>): Winner =
    let memory = System.Collections.Generic.HashSet<(int list) * (int list)> ()

    let rec playRoundUntilWin (p1: Queue<int>) (p2: Queue<int>): Winner =
        if p1.Length = 0 then
            Player2 (List.ofSeq p2)
        else if p2.Length = 0 then
            Player1 (List.ofSeq p1)
        else if memory.Contains(List.ofSeq p1, List.ofSeq p2) then
            Player1 (List.ofSeq p1)
        else
            memory.Add (List.ofSeq p1, List.ofSeq p2) |> ignore
            match p1.Uncons, p2.Uncons with
            | (h1,t1), (h2,t2) when t1.Length >= h1 && t2.Length >= h2 ->
                match playRecursiveCombat (Seq.take h1 t1, Seq.take h2 t2) with
                | Player1 _ -> playRoundUntilWin (t1.Conj(h1).Conj(h2)) t2
                | Player2 _ -> playRoundUntilWin t1 (t2.Conj(h2).Conj(h1))
            | (h1,t1), (h2,t2) when h1 > h2 -> playRoundUntilWin (t1.Conj(h1).Conj(h2)) t2
            | (h1,t1), (h2,t2) when h1 < h2 -> playRoundUntilWin t1 (t2.Conj(h2).Conj(h1))
            | _ -> failwithf "apparently two cards have the same value"

    playRoundUntilWin (Queue.ofSeq player1) (Queue.ofSeq player2)
    

[<EntryPoint>]
let main argv =
    let calcScore (cards: int list) =
        cards
        |> List.rev
        |> List.indexed
        |> List.map (fun (i, v) -> (i+1) * v)
        |> List.reduce (+)


    // part 1
    getData argv
    |> playGameUntilEnd1
    ||> fun l r ->
        match l.Length, r.Length with
        | a, 0 -> l
        | 0, a -> r
        | _ -> failwith "apparently the game stopped before it was finished"
    |> calcScore
    |> printfn "%d"
   
    // part 2
    getData argv
    |> playRecursiveCombat
    |> fun w -> w.Value
    |> calcScore
    |> printfn "%d"

    0
