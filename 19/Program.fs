open ActivePatterns
open Lib
open System

type Symbol =
    | Terminal of char
    | Nonterminal of int
    with override x.ToString () = match x with | Terminal c -> c.ToString () | Nonterminal i -> i.ToString ()
type Production = Symbol list
type Rule = int * (Production list)
type Grammar = {
    Rules: Map<int, Production list>
    Input: string list
}

/// production list & index of production & index of element in production
type ParserState =
    | Solved
    | InProcess of InProcessParserState
and InProcessParserState = struct
        val Production: Production
        val Index: int
        val CurrSymbol: Symbol
        val PreviousStates: InProcessParserState list
        val private rules: Map<int, Production list>

        new(production, index, previousStates, rules) = {
            Production = production
            Index = index
            CurrSymbol = production.[index]
            PreviousStates = previousStates
            rules = rules
        }

        static member getAllParsingStateForStartOfRule (rules: Map<int, Production list>) (statesStack: InProcessParserState list) (idx: int) = seq {
            for production in rules.[idx] do
                yield new InProcessParserState(production, 0, statesStack, rules)
        }

        member x.next () =
            if x.Index + 1 < x.Production.Length then
                let r = InProcess (InProcessParserState(x.Production, (x.Index + 1), x.PreviousStates, x.rules))
                //printfn "%s -> %s" (x.ToString ()) (r.ToString ())
                r
            else
                if x.PreviousStates.Length <> 0 then
                    let r = x.PreviousStates.Head.next ()
                    //printfn "%s ^-> %s" (x.ToString ()) (r.ToString ())
                    r
                else
                    //printfn "%s ** solved" (x.ToString ())
                    Solved

        member x.tryParse (ch: char) =
            match x.CurrSymbol with
            | Terminal c when c = ch -> Seq.replicate 1 (x.next ())
            | Terminal _ -> Seq.empty
            | Nonterminal idx -> 
                InProcessParserState.getAllParsingStateForStartOfRule x.rules (x::x.PreviousStates) idx
                |> Seq.collect (fun state -> state.tryParse ch)

        override x.ToString () = sprintf "%s: pos %d (%A) %d parents" (String.Join(" ", x.Production)) x.Index x.CurrSymbol (x.PreviousStates.Length)
    end


let getData argv =
    let parseRules (ruleStrings: string seq) =
        let parseSingleRule (string: string): Rule =
            let productionSeparatorIdx = string.IndexOf ':'
            let id =
                string.[.. productionSeparatorIdx - 1]
                |> Int32.Parse

            let production =
                string.[productionSeparatorIdx + 2 ..].Split ' '
                |> Seq.splitWhen ((=) "|")
                |> Seq.map (Seq.map (fun str ->
                    match str with
                    | MatchesRegex "\"(\w+)\"" v -> Terminal v.[0]
                    | MatchesRegex "(\d+)" referencedId -> Nonterminal (Int32.Parse referencedId)
                    | _ -> failwithf "could not parse %s" str
                ))
                |> Seq.map (List.ofSeq)
                |> List.ofSeq

            (id, production)

        ruleStrings
        |> Seq.map parseSingleRule
        |> Map.ofSeq

    let parse (rulesSection: string seq) (inputSection: string seq) = {
        Rules = parseRules rulesSection
        Input = List.ofSeq inputSection
    }
        
    readEmptyLineSeparatedSections (getFileName argv)
    |> (fun se -> Seq.head se, se |> Seq.tail |> Seq.head)
    ||> parse

let stringIsValid (rules: Map<int, Production list>) (str: string) =
    let startingStates =
        InProcessParserState.getAllParsingStateForStartOfRule rules List.empty 0
        |> Seq.map InProcess

    str
    |> Seq.fold (fun states ch -> seq {
            for state in states do
                match state with
                | InProcess s -> yield! (s.tryParse ch)
                | _ -> ()
        }) startingStates
    |> Seq.contains Solved

[<EntryPoint>]
let main argv =
    let data = getData argv

    // part 1
    data.Input
    |> Seq.filter (stringIsValid data.Rules)
    |> Seq.length
    |> printfn "%d"

    // part 2
    let updatedRules =
        data.Rules
            .Add(8, [ [ Nonterminal 42 ]; [ Nonterminal 42; Nonterminal 8 ] ])
            .Add(11, [ [ Nonterminal 42; Nonterminal 31 ]; [Nonterminal 42; Nonterminal 11; Nonterminal 31]])

    data.Input
    |> Seq.filter (stringIsValid updatedRules)
    |> Seq.length
    |> printfn "%d"
    0
