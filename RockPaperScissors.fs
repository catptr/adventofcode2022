module RockPaperScissors

open System.IO
open Common

type Move = Rock | Paper | Scissors
type Outcome = Loss | Win | Draw
type Round = { Opponent: Move; You: Move }

let toMove c =
    match c with
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | "C" | "Z" -> Scissors
    | _ -> sprintf "Unknown move '%s'" c
           |> failwith

let toOutcome c =
    match c with
    | "X" -> Loss
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> sprintf "Unknown outcome '%s'" c
           |> failwith

let findMove m o =
    let lose =
        function Rock -> Scissors
               | Paper -> Rock
               | Scissors -> Paper
    let win =
        function Rock -> Paper
               | Paper -> Scissors
               | Scissors -> Rock
    match o with
    | Loss -> lose m
    | Win -> win m
    | Draw -> m

let round o y =
    { Opponent = o; You = y }

let round' m o =
    findMove m o |> round m

let calculateScore (r: Round) =
    let moveScore =
        match r.You with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3
    let outcomeScore = 
        match r with
        // Opponent's win
        | { Opponent=Rock; You=Scissors }
        | { Opponent=Paper; You=Rock }
        | { Opponent=Scissors; You=Paper } -> 0
        // Your win
        | { Opponent=Rock; You=Paper }
        | { Opponent=Paper; You=Scissors }
        | { Opponent=Scissors; You=Rock } -> 6
        // Draw
        | _ -> 3
    moveScore + outcomeScore

let lineToRound f (l: string) =
    match l.Split(' ') with
    | [|a; b|] -> f a b
    | _ -> failwith <| sprintf "Couldn't parse line '%s'" l

let solve (path: string) (puzzle: Puzzle) =
    let solution parse ls =
        ls
        |> Array.map (splitPair ' ' >> parse >> calculateScore)
        |> Array.sum
    
    printfn "Day 2:"
    
    let input = File.ReadAllLines(path)

    if isSet puzzle Puzzle.First then
        printfn "Solution to first puzzle: %d" <| solution (fun (a,b) -> round (toMove a) (toMove b)) input
    if isSet puzzle Puzzle.Second then
        printfn "Solution to second puzzle: %d" <| solution (fun (a,b) -> round' (toMove a) (toOutcome b)) input
