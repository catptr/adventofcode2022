module NoSpaceLeftOnDevice

open System
open System.IO
open Common

type Input =
    | Cd of string
    | Ls
    | Dir of string
    | File of int * string

type Entry =
    | Directory of name: string * children: Entry list
    | File of name: string * size: int

let parse (l: string) =
    let command (ps: string[]) =
        let c = ps.[0]
        match c with
        | "cd" -> Cd ps.[1]
        | "ls" -> Ls
        | _ -> failwith <| sprintf "expected cd or ls, got '%s'" c
    
    let file (ps: string[]) =
        Input.File (int ps.[0], ps.[1])
    
    let parts = l.Split(' ')
    match parts.[0] with
    | "$" -> Array.tail parts |> command
    | "dir" -> parts.[1] |> Dir
    | d when Char.IsDigit(d, 0) -> parts |> file 
    | _ -> failwith <| sprintf "unexpected terminal output: '%s'" l

let tree ls =
    let mapDir f d =
        match d with
        | Directory (a,b) -> (a,b) |> f |> Directory
        | _ -> d

    let addToDirectory dir entry =
        mapDir (fun (p,cs) -> p,entry :: cs) dir

    let mapTop f stack =
        match stack with
        | x :: xs -> f x :: xs
        | [] -> []
    
    let reduceTop2 f stack =
        match stack with
        | x :: y :: rest -> f x y :: rest
        | _ -> stack

    let flip f a b =
        f b a
    
    let inputToEntry stack line =
        match line with
        | Input.File (s,n) -> mapTop (fun d -> File (n,s) |> addToDirectory d) stack
        | Input.Cd   ".."  -> reduceTop2 (flip addToDirectory) stack
        | Input.Cd   path  -> Directory (path,[]) :: stack
        | Input.Dir  _
        | Input.Ls         -> stack

    List.fold inputToEntry [] ls
    |> List.reduce (flip addToDirectory)

let folderSizes tree =
    // lol... lmao...
    let rec size e =
        match e with
        | File (n,s) -> false,n,s,[]
        | Directory (n,cs) -> let childResults = List.map size cs
                              let totalSize = List.sumBy (fun (_,_,s,_) -> s) childResults
                              let folders = List.filter (fun (b,_,_,_) -> b) childResults
                                            |> List.map (fun (_,n,s,fs) -> n,s,fs)
                              
                              let childReturns  = List.collect (fun (_,_,fs) -> fs) folders

                              let thisFolder = n,totalSize
                              let toReturn = thisFolder :: childReturns
                              
                              true,n,totalSize,toReturn

    size tree
    |> fun (_,_,_,fs) -> fs

let solve (path: string) (puzzle: Puzzle) =
    printfn "Day 7:"

    let fileTree =
        File.ReadAllLines(path)
        |> List.ofArray
        |> List.map parse
        |> tree

    let fs = folderSizes fileTree
             |> List.map snd
             |> List.sortDescending
    
    if isSet puzzle Puzzle.First then
        fs |> List.filter ((>=) 100000) |> List.sum 
        |> printfn "Solution to first puzzle: %d"
    
    if isSet puzzle Puzzle.Second then
        let diskSize = 70000000
        let neededForUpdate = 30000000
        let usedSize = List.head fs
        let unusedSize = diskSize - usedSize
        let needToFree = neededForUpdate - unusedSize

        fs |> List.findBack ((<=) needToFree)
        |> printfn "Solution to second puzzle: %d"
