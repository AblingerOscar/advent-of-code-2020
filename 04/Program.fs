open Lib
open System
open System.Text.RegularExpressions

let tupleAdd (a, b) c = a, b, c
let addTuple a (b, c) = a, b, c

let requiredEntries = Set.ofList [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]
let requiredEntryRegex = Map.empty
                            .Add("byr", Regex "^(192[0-9]|19[3-9][0-9]|200[012])$")
                            .Add("iyr", Regex "^(201[0-9]|2020)$")
                            .Add("eyr", Regex "^(202[0-9]|2030)$")
                            .Add("hgt", Regex "^(1([5-8][0-9]|9[0-3])cm|(59|6[0-9]|7[0-6])in)$")
                            .Add("hcl", Regex "^(#[0-9a-f]{6})$")
                            .Add("ecl", Regex "^(amb|blu|brn|gry|grn|hzl|oth)$")
                            .Add("pid", Regex "^([0-9]{9})$")
                            .Add("cid", Regex "^(.*)$")


let requiredEntryFunc = Map.empty
                            .Add("byr", fun (v: string) ->
                                match Int32.TryParse v with
                                | true, i -> 1920 <= i && i <= 2002 && v.Length = 4
                                | _ -> false
                            )
                            .Add("iyr", fun (v: string) ->
                                match Int32.TryParse v with
                                | true, i -> 2010 <= i && i <= 2020 && v.Length = 4
                                | _ -> false
                            )
                            .Add("eyr", fun (v: string) ->
                                match Int32.TryParse v with
                                | true, i -> 2020 <= i && i <= 2030 && v.Length = 4
                                | _ -> false
                            )
                            .Add("hgt", fun (v: string) ->
                                let parsed = Int32.TryParse (v.Substring(0, (v.Length-2)))

                                match tupleAdd parsed (v.Substring(v.Length - 2, 2)) with
                                    | true, i, "cm" -> 150 <= i && i <= 193
                                    | true, i, "in" -> 59 <= i && i <= 76
                                    | _ -> false
                            )
                            .Add("hcl", fun v -> v.Length = 7 && v.[0] = '#' && Seq.forall (fun c ->
                                        Set.ofList [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f' ]
                                        |> Set.contains c) (v.Substring(1))
                                    )
                            .Add("ecl", fun v ->
                                Set.empty.Add("amb").Add("blu").Add("brn").Add("gry").Add("grn").Add("hzl").Add("oth")
                                    .Contains(v))
                            .Add("pid", fun v -> v.Length = 9 && Seq.forall Char.IsDigit v)
                            .Add("cid", fun _ -> true)

let allEntries = requiredEntries.Add "cid"
let twoNewlinesRegex = Regex("\n\r\n\r|\n\n|\r\r|\r\n\r\n")

let getData argv = seq {
    let text = System.IO.File.ReadAllText (getFileName argv)

    yield! twoNewlinesRegex.Split text
            |> Seq.map (fun entries ->
                entries.Split (' ', '\n')
                |> Seq.map (fun entry ->
                    let kvp = entry.Split ':'
                    (kvp.[0].Trim (), kvp.[1].Trim ())
                )
                |> Map.ofSeq
            )
}

let isValid1 (passport: Map<string, string>) =
    let keys = (passport |> Map.toSeq |> Seq.map fst |> Set.ofSeq)
    requiredEntries.IsSubsetOf keys && keys.IsSubsetOf allEntries

let isValid2 (passport: Map<string, string>) =
    let keys = (passport |> Map.toSeq |> Seq.map fst |> Set.ofSeq)

    requiredEntries.IsSubsetOf keys && keys.IsSubsetOf allEntries
        && Map.toList passport
            |> Seq.forall (fun (key, value) ->
                (requiredEntryRegex.Item key).IsMatch value
            )

let isValid3 (passport: Map<string, string>) =
    let keys = (passport |> Map.toSeq |> Seq.map fst |> Set.ofSeq)

    requiredEntries.IsSubsetOf keys && keys.IsSubsetOf allEntries
        && Map.toList passport
            |> Seq.forall (fun (key, value) ->
                (requiredEntryFunc.Item key) value
            )


[<EntryPoint>]
let main argv =
    let numberOfValidPassports =
        getData argv
        |> Seq.filter isValid2
        |> Seq.length
    
    printfn "Found %d valid passports" numberOfValidPassports

    waitForUser ()
    0 // return an integer exit code