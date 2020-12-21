open ActivePatterns
open Lib
open System

let getData argv =
    System.IO.File.ReadLines (getFileName argv)
    |> Seq.map (fun line ->
        match line with
        | MatchesRegex2 "([^(]+) \(contains (.*)\)" (ingredients, allergens) -> ingredients.Split ' ', allergens.Split ", "
        | _ -> failwithf "Couldn't parse line %s" line)

let foldPossibleIngredients (mergeF: Set<string> -> Set<string> -> Set<string>) (map: Map<string, Set<string>>) (ingredients, allergens): Map<string, Set<string>> =
    Seq.fold (fun m allergen ->
        if m.ContainsKey allergen then
            m.Add(allergen, mergeF m.[allergen] ingredients)
        else
            m.Add(allergen, ingredients)
    ) map allergens

let intersectPossibleIngredients = foldPossibleIngredients Set.intersect

let rec assignAllAllergens (map: Map<string, Set<string>>) = seq {
        let assignedAllergens = 
            map
            |> Map.toSeq
            |> Seq.filter (snd >> Set.count >> (=) 1)
            |> Seq.map (fun (ingr, all) -> ingr, all.MinimumElement)

        yield! assignedAllergens

        let toRemove =
            assignedAllergens
            |> Seq.map snd
            |> Set.ofSeq

        let mapWithoutAssignedAllergens =
            map
            |> Map.map (fun _ allergenes -> Set.difference allergenes toRemove)
            |> Map.filter (fun _ allergenes -> not allergenes.IsEmpty)

        if mapWithoutAssignedAllergens.Count <> 0 then
            yield! assignAllAllergens mapWithoutAssignedAllergens
    }

[<EntryPoint>]
let main argv =
    let data = Seq.cache (getData argv)

    let allergensAndPotContainers =
        data
        |> Seq.map (fun (ingredients, allergens) -> (Set.ofArray ingredients, allergens))
        |> Seq.fold intersectPossibleIngredients Map.empty
        |> Map.toSeq

    let potAllergenContainers =
        allergensAndPotContainers
        |> Seq.map snd
        |> Set.unionMany

    let inertIngredients =
        data
        |> Seq.collect fst
        |> Seq.filter (potAllergenContainers.Contains >> not)

    let inertIngredientsSet = Set.ofSeq inertIngredients
    
    // part 1
    inertIngredients
    |> Seq.length
    |> printfn "%d"

    // part 2
    let removeIngredients toRemove (ingredients, allergens) = (Set.difference ingredients toRemove, allergens)

    let allergenMap =
        data
        |> Seq.map (fun (ingredients, allergens) -> (Set.ofArray ingredients, allergens))
        |> Seq.map (removeIngredients inertIngredientsSet)
        |> Seq.fold intersectPossibleIngredients Map.empty

    assignAllAllergens allergenMap
    |> Seq.sortBy fst
    |> Seq.map snd
    |> fun ingr -> String.Join(",", ingr)
    |> printfn "%s"

    
    0
