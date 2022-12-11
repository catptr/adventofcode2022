module RucksackReorganization

open System.IO
open Common

type Rucksack = { A:Set<char>; B:Set<char> }

let rucksack (l: string) =
    let mid = l.Length / 2
    { A = Set(l.Substring(0,mid))
      B = Set(l.Substring(mid)) }

let priority (c:char) =
    let t = int c
    let a = int 'a'

    if t < a then
        t - 38
    else
        t - 96

let solve (path: string) (puzzle: Puzzle) =
    let getMisplaced rs =
        let getCommonType = (fun r -> Set.intersect r.A r.B) >> Seq.exactlyOne
        rs
        |> Array.map getCommonType

    let getBadges rs =
        rs
        |> Array.map (fun r -> Set.union r.A r.B)
        |> Array.chunkBySize 3
        |> Array.map (Array.reduce Set.intersect >> Seq.exactlyOne)

    let solution f rs =
        f rs
        |> Array.sumBy priority

    printfn "Day 3:"

    let rucksacks = File.ReadAllLines(path)
                    |> Array.map rucksack

    if isSet puzzle Puzzle.First then
        printfn "Solution to first puzzle: %d" <| solution getMisplaced rucksacks
    
    if isSet puzzle Puzzle.Second then
        printfn "Solution to second puzzle: %d" <| solution getBadges rucksacks
