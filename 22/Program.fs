open Lib
open System
open System.Collections.Generic

let getData argv =
    readEmptyLineSeparatedSections (getFileName argv)
    |> Seq.map (Seq.skip 1)
    |> Seq.map (Seq.map Int32.Parse)
    |> (fun sequences -> (Seq.head sequences, Seq.head (Seq.tail sequences)))

let playGameUntilEnd ((player1, player2): seq<int> * seq<int>): list<int> * list<int> =
    // ugly procedural code :(
    let queue1 = Queue<int> player1
    let queue2 = Queue<int> player2
    
    let rec play (): unit =
        if queue1.Count = 0 || queue2.Count = 0 then
            ()
        else
            match queue1.Dequeue(), queue2.Dequeue() with
            | x, y when x > y ->
                queue1.Enqueue x
                queue1.Enqueue y
                play ()
            | x, y when x < y ->
                queue2.Enqueue y
                queue2.Enqueue x
                play ()
            | _ -> failwithf "apparently two cards have the same value"

    play ()

    (List.ofSeq queue1, List.ofSeq queue2)

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
