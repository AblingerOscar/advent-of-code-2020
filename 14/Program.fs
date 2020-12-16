open ActivePatterns
open Lib
open System

// todo: switch to long (so that it is actually long enough to fit entire mask)

type Mask =
    struct
        val ones: int64
        val zeroes: int64

        new (mask: string) =
            let applyChToNumber (ones, zeroes) (i, ch) =
                match ch with
                | 'X' -> ones, zeroes
                | '0' -> ones, zeroes + (1L <<< i)
                | '1' -> ones + (1L <<< i), zeroes
                | _ -> failwithf "invalid mask character found: '%c'" ch
 
            let (o, z) =
                mask
                |> Seq.rev
                |> Seq.indexed
                |> Seq.fold applyChToNumber (0L, 0L)

            {ones = o; zeroes = z}
            
        member x.apply (n: int64): int64 = ~~~(~~~(n ||| x.ones) ||| x.zeroes)

        static member none = Mask ""
    end

type Statement =
    | MaskDefinition of string
    | MemoryAssignment of int * int64

let getData argv =
    System.IO.File.ReadLines (getFileName argv)
    |> Seq.map (fun line ->
        match line with
        | MatchesRegex "mask = ((X|0|1)+)" m -> MaskDefinition m
        | MatchesRegex2 "mem\[(\d+)\] = (\d+)" (pos, value) -> MemoryAssignment (int pos, int64 value)
        | _ -> failwithf "line could not be matched: %s" line
    )
    
[<EntryPoint>]
let main argv =
    let folder ((mask, memory): Mask * Map<int, int64>) (statement: Statement) =
        match statement with
        | MaskDefinition newMask -> Mask newMask, memory
        | MemoryAssignment (pos, value) -> (mask, memory.Add(pos, (mask.apply value)))

    getData argv
    |> Seq.fold folder (Mask.none, Map.empty)
    |> snd
    |> Map.fold (fun state _ v -> state + v) 0L
    |> printfn "%d"

    0
