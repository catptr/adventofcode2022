module CalorieCounting

open System.IO
open Common

let solve (path: string) (puzzle: Puzzle) =
    let caloriesPerElf (i: string) =
        i.Split("\n\n")
        |> Array.map (fun c -> c.Split("\n") |> Array.map int |> Array.sum)
        
    printfn "Day 1:"

    let input = File.ReadAllText(path)
    
    if isSet puzzle Puzzle.First then
        printfn "Solution to first puzzle: %d" <| Array.max (caloriesPerElf input)
    if isSet puzzle Puzzle.Second then
        printfn "Solution to second puzzle: %d" (caloriesPerElf input |> Array.sortDescending |> Array.take 3 |> Array.sum)