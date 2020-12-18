open ActivePatterns
open Lib
open System

type Token =
    | Plus
    | Mult
    | Number of int64
    | LParens
    | RParens

let getData argv =
    let tokenize line = [
        for char in line do
            match char with
            | ' ' -> () // do nothing
            | '(' -> yield LParens
            | ')' -> yield RParens
            | '*' -> yield Mult
            | '+' -> yield Plus
            | n when '0' <= n && n <= '9' -> yield Number (Char.GetNumericValue n |> int64)
            | _ -> failwithf "unknown character: '%c'" char
    ]

    System.IO.File.ReadLines (getFileName argv)
    |> Seq.map tokenize

let evaluate1 (tokens: Token list) =
    let stackCalc (stack: int64 list) (f: int64 -> int64 -> int64): int64 list =
        match stack with
        | a::b::t -> (f a b)::t
        | _ -> failwithf "stackunderflow"

    let rec calc (valStack: int64 list) (funcStack: (int64 -> int64 -> int64) list) (tokens: Token list) =
        match tokens with
        | [] -> valStack.Head
        | Plus::t -> calc valStack ((+)::funcStack) t
        | Mult::t -> calc valStack ((*)::funcStack) t
        | (Number n)::t -> calc (stackCalc (n::valStack) funcStack.Head) funcStack.Tail t
        | LParens::t -> calc (0L::valStack) ((+)::funcStack) t
        | RParens::t -> calc (stackCalc valStack funcStack.Head) funcStack.Tail t

    calc [0L] [(+)] tokens

[<EntryPoint>]
let main argv =
    getData argv
    |> Seq.map evaluate1
    |> Seq.reduce (+)
    |> printfn "%d"

    0
