module TuningTrouble

open System.IO
open Common

type Marker = Packet | Message

let tryFindMarker marker stream =
    let size = 
        match marker with
        | Packet -> 4
        | Message -> 14
    
    List.windowed size stream
    |> List.map (Set.ofList >> Set.count)
    |> List.tryFindIndex ((=) size)
    |> Option.map ((+) size)

let solve (path: string) (puzzle: Puzzle) =
    printfn "Day 6:"

    let stream = File.ReadAllText(path)
    
    let solution marker stream =
        Seq.toList stream
        |> tryFindMarker marker
        |> Option.map string
        |> Option.defaultValue "None found" 
    
    if isSet puzzle Puzzle.First then
        printfn "Solution to first puzzle: %s" <| solution Packet stream
    if isSet puzzle Puzzle.Second then
        printfn "Solution to second puzzle: %s" <| solution Message stream
