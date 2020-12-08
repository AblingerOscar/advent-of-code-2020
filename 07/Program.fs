open Lib

type BagWithCount = int * string
type Bags = BagWithCount list
type BagDefinition = string * Bags

let getData () : seq<BagDefinition> =
    let description = "(?<name>\w+ \w+)"
    let inner = sprintf "^ *(?<amount>\d+) %s bags?" description
    let regex = sprintf "^(?<container>\w+ \w+) bags contain (no other bags|(?<bags>\d+ %s bags?(, \d+ %s bags?)*))\.$" description description

    let transformBags (bags: string) =
        bags.Split ","
        |> Seq.map (regexMap inner (fun map -> (int map.["amount"]), map.["name"]))
        |> Seq.toList

    System.IO.File.ReadLines "data/source.txt"
    |> Seq.map (regexMap regex (fun map ->
        let bags = match map.Item "bags" with
                    | "" -> List.empty
                    | bags -> (transformBags bags)
        map.["container"], bags))

// easy part 1
let rec bagContains name (bagDefs: Map<string, Bags>) bag =
    let innerBagNames =
        bagDefs.[bag]
        |> Seq.map snd
    let directlyContains = Seq.exists ((=) name) innerBagNames

    directlyContains || Seq.exists (bagContains name bagDefs) innerBagNames

// easy part 2
let rec recCalcContents (bagDefs: Map<string, Bags>) container =
    if bagDefs.[container].Length = 0 then
        1
    else 
        1 + (
            bagDefs.[container]
            |> Seq.map (fun (cnt, content) -> cnt * (recCalcContents bagDefs content))
            |> Seq.reduce (+)
        )

[<EntryPoint>]
let main argv =
    let data = Seq.cache (getData ())
    let dataMap = (Map.ofSeq data)

    // part 1
    let nrOfBagsThatContainSG =
        data
        |> Seq.map fst
        |> Seq.filter (bagContains "shiny gold" (Map.ofSeq data))
        |> Seq.length
    printfn "Found %d bags containing a shiny gold bag" nrOfBagsThatContainSG

    // part 2
    let nrOfBagsThatSGContains = (recCalcContents dataMap "shiny gold") - 1 // -1 for the shiny gold bag itself

    printfn "Found %d bags in the shiny gold bag" nrOfBagsThatSGContains
    Lib.waitForUser ()
    0 // return an integer exit code
