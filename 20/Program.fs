open ActivePatterns
open Lib
open System

let getData argv =
    let parseSection (str: string seq) =
        let id =
            match Seq.head str with
            | MatchesRegex "Tile (\d+):" id -> Int32.Parse id
            | _ -> failwithf "could not match id line: '%s'" (Seq.head str)
        (id, str |> Seq.tail |> List.ofSeq)

    readEmptyLineSeparatedSections (getFileName argv)
    |> Seq.map parseSection

(*
type EdgeMaps = struct
    val ListTop: Map<string, int>
    val ListBottom: Map<string, int>
    val ListLeft: Map<string, int>
    val ListRight: Map<string, int>

    new(top, bottom, left, right) = { ListTop = top; ListBottom = bottom; ListLeft = left; ListRight = right }

    static member empty = EdgeMaps(Map.empty, Map.empty, Map.empty, Map.empty)

    member x.containsXMatchingEdges idx (top, bottom, left, right) =
        let a = [
                x.ListBottom.ContainsKey top;
                x.ListTop.ContainsKey bottom;
                x.ListRight.ContainsKey left;
                x.ListLeft.ContainsKey right
            ]
        a
        |> Seq.filter id
        |> Seq.length
end

let foldEdges (edges: EdgeMaps) ((idx, (top, bottom, left, right)): int * (string * string * string * string)) =
    EdgeMaps(
        edges.ListTop.Add(top, idx),
        edges.ListBottom.Add(bottom, idx),
        edges.ListLeft.Add(left, idx),
        edges.ListRight.Add(right, idx)
        )

*)

type Rotation =
    | NoRotation
    | Left90
    | UpsideDown
    | Right90

type Flipping =
    | NoFlipping
    | XFlipped
    | YFlipped

type ConnectedSection =
    {
        Top: Option<ConnectedSection>
        Bottom: Option<ConnectedSection>
        Left: Option<ConnectedSection>
        Right: Option<ConnectedSection>
        Rotation: Rotation
        Flipping: Flipping
        Text: string list
        Id: int
    }

    member x.rotate

    member x.flipX =
        let flip =
            match x.Flipping with
            | NoFlipping -> XFlipped
            | XFlipped -> NoFlipping
            | YFlipped -> 
        {
            x with Flipping
        }

    static member Default = {
            Top = None
            Bottom = None
            Left = None
            Right = None
            Rotation = NoRotation
            Flipping = NoFlipping
            Id = 0
            Text = List.empty
        }

let getLeftString (section: string list) =
    section
    |> Seq.map (fun str -> str.[0])
    |> String.Concat

let getRightString (section: string list) =
    section
    |> Seq.map (fun str -> str.[section.Length - 1])
    |> Seq.rev
    |> String.Concat

[<EntryPoint>]
let main argv =
    let data = getData argv

    let edges =
        data
        //|> Seq.map (fun (idx, section) -> (idx, (section.Head, List.last section, getVerticalString section 0, getVerticalString section (section.Length - 1))))
        |> Seq.map (fun (idx, section) -> (idx, [ section.Head; List.last section; getLeftString section; getRightString section]))

    let edgesToIds =
        edges
        |> Seq.collect (fun (idx, edges) -> Seq.map (fun edge -> (edge, idx)) edges)
        |> List.ofSeq
        |> List.groupBy fst
        |> List.map (fun (edge, edgesWithIdx) -> edge, List.map (fun (_, idx) -> idx) edgesWithIdx)
        |> Map.ofList

    data
    |> Seq.map (fun (id, content) -> { ConnectedSection.Default with Id = id; Text = content })
    |> Seq.map (fun section -> )


    (*
    let edgeMaps =
        edges
        |> Seq.fold foldEdges EdgeMaps.empty

    let a =
        edges
        |> Seq.map (fun (idx, edges) -> (idx, edgeMaps.containsXMatchingEdges idx edges))
        |> List.ofSeq

    let b = List.ofSeq edges

    a
    |> Seq.filter (snd >> ((=) 2))
    |> Seq.map fst
    |> Seq.map (printfn "%d")
    *)
    0
