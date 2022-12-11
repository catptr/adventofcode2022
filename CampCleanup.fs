module CampCleanup

open System.IO
open Common

let toSets (l: string) =
    let mapBoth f (a, b) = f a, f b
    
    let toSet (s: string) =
        let a,b = splitPair '-' s |> mapBoth int
        seq { a .. b }
        |> Set.ofSeq
    
    splitPair ',' l
    |> mapBoth toSet
    
let solve (path: string) (puzzle: Puzzle) =
    let contains (a, b) =
        if Set.count a < Set.count b then
            Set.isSubset a b
        else
            Set.isSubset b a

    let overlaps (a, b) =
        Set.intersect a b
        |> Set.isEmpty
        |> not

    let solution f i =
        i
        |> Array.map toSets
        |> Array.countBy f
        |> Array.find (fun (b,_) -> b=true)
        |> snd    

    printfn "Day 4:"

    let input = File.ReadAllLines(path)

    if isSet puzzle Puzzle.First then
        printfn "Solution to first puzzle: %d" <| solution contains input
    
    if isSet puzzle Puzzle.Second then
        printfn "Solution to second puzzle: %d" <| solution overlaps input
