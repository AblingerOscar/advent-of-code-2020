open Lib
open System
open System.Collections.Generic

type Instruction =
    | Nop of int
    | Acc of int
    | Jmp of int

type ProgramResult =
    | Loop of int
    | Success of int


let getData argv =
    let mapToInstruction (map: Map<string, string>) =
        match (map.["instruction"], map.["number"]) with
        | ("nop", n) -> Nop (Int32.Parse n)
        | ("acc", n) -> Acc (Int32.Parse n)
        | ("jmp", n) -> Jmp (Int32.Parse n)
        | _ -> failwith "Unknown instruction"

    System.IO.File.ReadLines (getFileName argv)
    |> Seq.map (regexMap "(?<instruction>(nop|acc|jmp)) (?<number>(\+|-)\d+)" mapToInstruction)
    |> Map.ofIndexSeq

let getAccAtEnd (instructions: Map<int, Instruction>) =
    let rec recGetAccAtLoop (instr: Map<int, Instruction>) i acc (knownSet: Set<int>) =
        if i >= instr.Count then
            Success acc
        else if knownSet.Contains i then
            Loop acc
        else
            match instr.Item i with
            | Nop _ -> recGetAccAtLoop instr (i+1) acc (knownSet.Add(i))
            | Acc n -> recGetAccAtLoop instr (i+1) (acc+n) (knownSet.Add(i))
            | Jmp n -> recGetAccAtLoop instr (i+n) acc (knownSet.Add(i))
    
    recGetAccAtLoop instructions 0 0 Set.empty


let tryAllChanges (instructions: Map<int, Instruction>) =
    let rec recTryAllChanges (instr: Map<int, Instruction>) i =
        let repl = match instr.[i] with
                    | Nop n -> Some (Jmp n)
                    | Jmp n -> Some (Nop n)
                    | Acc n -> None
        match repl with
        | None -> recTryAllChanges instr (i+1)
        | Some repl ->
            // if we have a replacement we use the original map and replace the given entry
            match getAccAtEnd (instructions.Add(i, repl)) with
            | Success acc -> acc
            | Loop _ -> recTryAllChanges instr (i+1)

    recTryAllChanges instructions 0

[<EntryPoint>]
let main argv =
    getData argv
    |> tryAllChanges
    |> printf "acc of the fixed program is %d\n"

    waitForUser ()
    0
