﻿module ActivePatterns

open System.Text.RegularExpressions

let (|MatchesRegex|_|) (regex: string) (input: string) =
    let m = Regex.Match(input, regex)
    if m.Success then
        Some (m.Groups.Item 1).Value
    else
        None

let (|MatchesRegex2|_|) (regex: string) (input: string) =
    let m = Regex.Match(input, regex)
    if m.Success then
        Some ((m.Groups.Item 1).Value, (m.Groups.Item 2).Value)
    else
        None

