open Lib
open ActivePatterns
open System

type Notes (rules: Map<string, (int * int) list>,
            myTicket: int list,
            nearbyTickets: int list list) =
    member x.Rules = rules
    member x.MyTicket = myTicket
    member x.NearbyTickets = nearbyTickets

    member x.isInAnyRule n =
        x.Rules
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.collect id
        |> Seq.exists (fun (min, max) -> n >= min && n <= max)

let parseRules lines =
    lines
    |> Seq.map (fun line ->
        match line with
        | MatchesRegexAll "([^:]+): (\d+)-(\d+) or (\d+)-(\d+)" (name::min1::max1::min2::max2::tail) ->
            name, [int min1, int max1; int min2, int max2]
        | _ -> failwith "couldn't parse rule in line: '%s'" line
    )
    |> Map.ofSeq

let parseTicket (line: string) =
    line.Split ','
    |> Seq.map int
    |> Seq.toList

let parseTickets (str: string seq) = seq {
    for line in str do
        yield parseTicket line
}

let getData argv =
    match getFileName argv
            |> readEmptyLineSeparatedSections
            |> List.ofSeq with
    | rules::myTicket::nearbyTickets::_ -> 
        Notes(
            parseRules rules, 
            parseTicket (myTicket |> Seq.tail |> Seq.head),
            List.ofSeq (parseTickets (nearbyTickets |> Seq.tail))
        )
    | _ -> failwith "invalid input"

let rec assignColumnsTheirRules (columnsWithMatchingRules: (int * Set<string>) seq) = seq {
    if (Seq.length columnsWithMatchingRules) > 0 then

        let definitelyAssignedColumns =
            columnsWithMatchingRules
            |> Seq.choose (fun (idx, rules) ->
                match Set.toList rules with
                | [onlyRule] -> Some (idx, onlyRule)
                | _ -> None
            )
        
        yield! definitelyAssignedColumns

        let assignedRules =
            definitelyAssignedColumns
            |> Seq.map snd
            |> Set.ofSeq
        
        let assignedColumns =
            definitelyAssignedColumns
            |> Seq.map fst
            |> Set.ofSeq

        let leftoverColumns =
            columnsWithMatchingRules
            |> Seq.filter (fun (idx, rules) -> not (assignedColumns.Contains idx))
            |> Seq.map (fun (idx, rules) -> (idx, Set.difference rules assignedRules))
            |> Seq.cache
          
        yield! assignColumnsTheirRules (Seq.cache leftoverColumns)
}

[<EntryPoint>]
let main argv =
    let notes = getData argv
    
    // part 1
    notes.NearbyTickets
    |> Seq.collect id
    |> Seq.filter (notes.isInAnyRule >> not)
    |> Seq.fold (+) 0
    |> printfn "%d"

    // part 2
    let validTickets =
        notes.NearbyTickets
        |> Seq.filter (Seq.forall (notes.isInAnyRule))
        |> Seq.toList


    let ruleMatches value name (ranges: (int*int) list) =
        ranges
        |> Seq.exists (fun (min, max) -> min <= value && value <= max)

    let indicesWithMatchingRules = seq {
        for ticketIdx in 0 .. validTickets.Length - 1 do
            for fieldIdx in 0 .. validTickets.[ticketIdx].Length - 1 do
                let value = validTickets.[ticketIdx].[fieldIdx]

                yield (
                    fieldIdx, 
                    notes.Rules
                    |> Map.filter (ruleMatches value)
                    |> Map.toSeq
                    |> Seq.map fst
                    |> Set.ofSeq
                )
    }

    indicesWithMatchingRules
    |> Seq.groupBy fst
    |> Seq.map (fun (fIdx, fIdxAndRulesList) -> fIdx, Seq.map snd fIdxAndRulesList)
    |> Seq.map (fun (fIdx, rules) -> fIdx, Set.intersectMany rules)
    |> assignColumnsTheirRules
    |> Seq.filter (fun (_, rule) -> rule.StartsWith "departure")
    |> Seq.map fst
    |> Seq.map (fun i -> notes.MyTicket.[i])
    |> Seq.map int64
    |> Seq.fold (*) 1L
    |> printfn "%d"

    0
