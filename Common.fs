module Common

[<System.FlagsAttribute>]
type Puzzle =
    | None = 0
    | First = 1
    | Second = 2
    | Both = 3

let isSet (p: Puzzle) (f: Puzzle) =
    p &&& f <> Puzzle.None

let splitPair (c: char) (s: string) =
    match s.Split(c) with
    | [|a; b|] -> a,b
    | _ -> failwith <| sprintf "Couldn't split line '%s'" s

let mapBoth f (a, b) = f a, f b