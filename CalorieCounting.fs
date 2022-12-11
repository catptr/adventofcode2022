module CalorieCounting

open System.IO
open Common

let solve (path: string) (puzzle: Puzzle) =        
    printfn "Day 1:"

    let caloriesPerElf =
        File.ReadAllText(path).Split("\n\n")
        |> Array.map (fun c -> c.Split("\n") |> Array.map int |> Array.sum)
    
    if isSet puzzle Puzzle.First then
        printfn "Solution to first puzzle: %d" <| Array.max caloriesPerElf
    if isSet puzzle Puzzle.Second then
        printfn "Solution to second puzzle: %d" (caloriesPerElf |> Array.sortDescending |> Array.take 3 |> Array.sum)