open System.Collections
open Lib

type Forest (bitArrays: BitArray list) =
    let height = bitArrays.Length
    let width = bitArrays.Head.Length
    member _.IsEnd y = y >= height
    member _.IsTree x y = bitArrays.[y].[x % width]

let getData argv = 
    let lines = System.IO.File.ReadLines (getFileName argv)

    Forest [
        for line in lines do
            yield BitArray [|
                for char in line do
                    match char with
                    | '#' -> true
                    | '.' -> false
                    | _ -> failwith "unrecognised char"
            |]
    ]


let step dx dy forest x y =
    let rec stepR (forest: Forest) x y cnt =
        if forest.IsEnd y then
            cnt
        else if forest.IsTree x y then
            stepR forest (x+dx) (y+dy) (cnt+1)
        else
            stepR forest (x+dx) (y+dy) cnt
    stepR forest x y 0

[<EntryPoint>]
let main argv =
    let forest = getData argv
    
    printfn "While sloping using the (3 1) pattern I hit %d trees" (step 3 1 forest 0 0)
    
    let nr = seq [ (1, 1); (3, 1); (5, 1); (7, 1); (1, 2) ]
              |> Seq.map (fun (dx, dy) -> step dx dy forest 0 0)
              |> Seq.reduce (fun a b -> a * b)

    printfn "Trying all patterns and multiplying them together results in %d" nr

    waitForUser()
    0 // return an integer exit code
