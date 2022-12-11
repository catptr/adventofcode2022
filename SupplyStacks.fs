module SupplyStacks

open System.IO
open Common

let solve (path: string) (puzzle: Puzzle) =
    printfn "Day 5:"

    // let input = File.ReadAllText(path)
    
    // if isSet puzzle Puzzle.First then
    //     printfn "Solution to first puzzle: %d" <| Array.max (caloriesPerElf input)
    // if isSet puzzle Puzzle.Second then
    //     printfn "Solution to second puzzle: %d" (caloriesPerElf input |> Array.sortDescending |> Array.take 3 |> Array.sum)