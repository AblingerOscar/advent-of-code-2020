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

let evaluate2 (tokens: Token list) =
    let replaceRanges (list: 'a list) (ranges: (int * int * 'a) list): 'a list =
        ranges
        |> List.rev
        |> List.fold (fun newTokens (l, r, v) -> newTokens.[..l - 1] @ [v] @ newTokens.[r + 1..]) list

    let calcWithNoParentesis tokens =
        let aggregateAdditions tokens =
            tokens
            |> List.choose (fun token ->
                match token with
                | Number n -> Some n
                | _ -> None)
            |> List.reduce (+)

        tokens
        |> List.splitWhen ((=) Mult)
        |> List.map aggregateAdditions
        |> List.reduce (*)
 
 
    let rec calculateWithParenthesis (tokens: Token list) =
        let innerParenthesisIndexes =
            tokens
            |> List.indexed
            |> List.choose (fun (i, token) ->
                match token with
                | LParens | RParens -> Some (i, token)
                | _ -> None
            )

        if innerParenthesisIndexes.IsEmpty then
            calcWithNoParentesis tokens
        else
            innerParenthesisIndexes
            |> List.pairwise
            |> List.filter (fun (a, b) -> (snd a, snd b) = (LParens, RParens))
            |> List.map (fun (a, b) -> (fst a, fst b))
            |> List.map (fun (l, r) -> l, r, calcWithNoParentesis tokens.[l + 1 .. r - 1])
            |> List.map (fun (l, r, v) -> (l, r, Number v))
            |> replaceRanges tokens
            |> calculateWithParenthesis

    calculateWithParenthesis tokens

let echo pattern value =
    printfn pattern value
    value

[<EntryPoint>]
let main argv =
    // part 1
    getData argv
    |> Seq.map evaluate1
    |> Seq.reduce (+)
    |> printfn "%d"

    // part 2
    getData argv
    |> Seq.map evaluate2
    |> Seq.reduce (+)
    |> printfn "%d"

    0
