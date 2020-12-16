open ActivePatterns
open Lib
open System

// todo: switch to long (so that it is actually long enough to fit entire mask)

type Mask =
    struct
        val ones: int64
        val zeroes: int64
        val floatingDigits: int list

        new (mask: string) =
            let applyChToNumber (ones, zeroes) (i, ch) =
                match ch with
                | 'X' -> ones, zeroes
                | '0' -> ones, zeroes + (1L <<< i)
                | '1' -> ones + (1L <<< i), zeroes
                | _ -> failwithf "invalid mask character found: '%c'" ch
 
            let indexedReversedMask =
                mask
                |> Seq.rev
                |> Seq.indexed

            let floatingDigits =
                indexedReversedMask
                |> Seq.filter (snd >> (=) 'X')
                |> Seq.map fst
                |> List.ofSeq

            let (o, z) =
                indexedReversedMask
                |> Seq.fold applyChToNumber (0L, 0L)

            {ones = o; zeroes = z; floatingDigits = floatingDigits}
            
        member x.apply (n: int64): int64 = ~~~(~~~(n ||| x.ones) ||| x.zeroes)

        member x.allVariatonsWithFloating (number: int64): int64 seq =
            let num = number ||| x.ones

            let rec recFloatIndex (n: int64) (indexes: int list) = seq {
                match indexes with
                | [] -> yield n
                | idx::tail ->
                    yield! recFloatIndex n tail
                    yield! recFloatIndex (n ^^^ (1L <<< idx)) tail
            }    

            recFloatIndex num x.floatingDigits

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

    // part 1
    getData argv
    |> Seq.fold folder (Mask.none, Map.empty)
    |> snd
    |> Map.fold (fun state _ v -> state + v) 0L
    |> printfn "%d"


    // part 2
    let rec addAtAllIndices (map: Map<int64, int64>) value (indices: int64 list) =
        match indices with
        | [] -> map
        | idx::tail -> addAtAllIndices (map.Add (idx, value)) value tail

    let folder2 ((mask, memory): Mask * Map<int64, int64>) (statement: Statement) =
        match statement with
        | MaskDefinition newMask -> Mask newMask, memory
        | MemoryAssignment (pos, value) ->
            let newMap =
                int64 pos
                |> mask.allVariatonsWithFloating
                |> List.ofSeq
                |> addAtAllIndices memory value

            (mask, newMap)

    getData argv
    |> Seq.fold folder2 (Mask.none, Map.empty)
    |> snd
    |> Map.fold (fun state _ v -> state + v) 0L
    |> printfn "%d"

    0
