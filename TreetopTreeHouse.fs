module TreetopTreeHouse

open System.IO
open Common

// Names are hard.
// This function takes the map and a point and
// returns a sequence of the height of the tree
// at that point and all the trees from
// the point to the edge, in all directions.
// We reverse left and top slices so
// that they all go from the point
// out toward the edges. 
let slices (m: int[,]) x y =
    let v = m[y,x]
    let row = m[y,*]
    let col = m[*,x]
    seq {
        v,row[..x-1] |> Array.rev
        v,row[x+1..]
        v,col[..y-1] |> Array.rev
        v,col[y+1..]
    }

let isVisibleFromOutside m (x,y) =
    slices m x y
    // Tree we're testing is visible if all trees 
    // in at least one direction are shorter.
    |> Seq.exists (fun (h,ts) -> Array.forall (fun t -> t < h) ts)

let scenicScore m (x,y) =
    let numVisible (h,ts) =
        ts
        // Find the closest tree blocking our view.
        // Index is how many trees we've passed
        // Add 1 to include the blocker.
        // If no match we see all of them.
        |> Array.tryFindIndex (fun t -> t >= h)
        |> Option.map ((+) 1)
        |> Option.defaultValue (Array.length ts)
    
    slices m x y
    |> Seq.map numVisible
    |> Seq.reduce (*)

let charToInt c = int c - int '0'

let solve (path: string) (puzzle: Puzzle) =
    printfn "Day 8:"

    let input = File.ReadAllText(path)

    let heightMap =
        (input, [[]]) ||> Seq.foldBack (fun c acc ->
            match c with
            | '\n' -> [] :: acc
            | _    -> acc |> mapHead (fun innerList -> charToInt c :: innerList))
        |> array2D
    
    let height = Array2D.length1 heightMap
    let width = Array2D.length2 heightMap

    let coords = (seq { 1..width-2 }, seq { 1..height-2 }) ||> Seq.allPairs
    
    if isSet puzzle Puzzle.First then
        let edgeTrees = 2 * (width + height) - 4
        let numVisible =
            coords
            |> Seq.countBy (isVisibleFromOutside heightMap)
            |> Seq.find (fun (b,_) -> b=true)
            |> snd
        
        printfn "Solution to first puzzle: %d" <| edgeTrees + numVisible
    
    if isSet puzzle Puzzle.Second then
        coords
        |> Seq.map (scenicScore heightMap)
        |> Seq.max
        |> printfn "Solution to second puzzle: %d"