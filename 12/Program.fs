﻿open Lib
open System

type Direction = int * int // left/right multiplier * up/down multiplier
type Ship = int * int * Direction // left/right * up/down * current boat direction

[<Struct>]
type Degree =
    | D90
    | D180
    | D270
    static member (~-) (d: Degree) =
        match d with
        | D90 -> D270
        | D180 -> D180
        | D270 -> D90

let parseDegree n =
    match Int32.Parse n with 
    | 90 -> D90
    | 180 -> D180
    | 270 -> D270
    | _ -> failwith "%s is not a valid direction change" n

type Instruction =
    | North of int
    | East of int
    | South of int
    | West of int
    | Left of Degree
    | Right of Degree
    | Forward of int

let getData argv =
    System.IO.File.ReadLines (getFileName argv)
    |> Seq.map (fun line ->
        match line.[0], line.[1..] with
        | 'N', n -> North (Int32.Parse n)
        | 'E', n -> East (Int32.Parse n)
        | 'S', n -> South (Int32.Parse n)
        | 'W', n -> West (Int32.Parse n)
        | 'L', n when n = "90" || n = "180" || n = "270" -> Left (parseDegree n)
        | 'R', n when n = "90" || n = "180" || n = "270" -> Right (parseDegree n)
        | 'F', n -> Forward (Int32.Parse n)
        | _ -> failwithf "invalid input '%s'" line
        )

let turnR (direction: Degree) ((x, y): int * int) =
        match direction with
        | D90  -> ( y, -x)
        | D180 ->(-x, -y)
        | D270 ->(-y,  x)

let move (ship: Ship) (instruction: Instruction): Ship =
    let (x, y, (dx, dy)) = ship
    match instruction with
    | North n -> x, y+n, (dx, dy)
    | East n -> x+n, y, (dx, dy)
    | South n -> x, y-n, (dx, dy)
    | West n -> x-n, y, (dx, dy)
    | Left direction -> x, y, (turnR -direction (dx, dy))
    | Right direction -> x, y, (turnR direction (dx, dy))
    | Forward n -> x+dx*n, y+dy*n, (dx, dy)

type ShipWithWaypoint = (int*int) * (int*int) // ship (x,y) * waypoint (x, y)

let move2 ((ship, (wx, wy)): ShipWithWaypoint) (instruction: Instruction): ShipWithWaypoint =
    let (sx, sy) = ship
    match instruction with
    | North n -> ship, (wx, wy+n)
    | East n -> ship, (wx+n, wy)
    | South n -> ship, (wx, wy-n)
    | West n -> ship, (wx-n, wy)
    | Left direction -> ship, (turnR -direction (wx, wy))
    | Right direction -> ship, (turnR direction (wx, wy))
    | Forward n -> ((sx + wx * n), (sy + wy*n)), (wx, wy)

[<EntryPoint>]
let main argv =
    let (x, y, _) =
        getData argv
        |> Seq.fold move (0, 0, (1, 0))

    printfn "%d" ((abs x) + (abs y))
    
    let ((x, y), _) =
        getData argv
        |> Seq.fold move2 ((0, 0), (10, 1))
    
    printfn "%d" ((abs x) + (abs y))
    
    0
