open System
open System.IO

let mapBoth f (a, b) = f a, f b

// let text = File.ReadAllText("day7.txt")
let text = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"

let input = text.Split('\n') |> Array.toList

type Input =
    | Cd of string
    | Ls
    | Dir of string
    | File of int * string

type Entry =
    | Directory of name: string * children: Entry list
    | File of name: string * size: int

module Entry =
    let mapDir f d =
        match d with
        | Directory (a,b) -> (a,b) |> f |> Directory
        | _ -> d

    let addToDirectory dir entry =
        mapDir (fun (p,cs) -> p,entry :: cs) dir

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
    | _ -> failwith "wtf"

let rec sortChildren t =
    let entrySort e1 e2 =
        let getName e =
            match e with
            | Directory (n, _) -> n
            | File (n, _) -> n
        
        (e1,e2) |> mapBoth getName ||> compare
    
    match t with
    | File _ -> t
    | Directory (n, cs) -> Directory (n, List.map sortChildren cs |> List.sortWith entrySort)

let tree ls =
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
        | Input.File (s,n) -> mapTop (fun d -> File (n,s) |> Entry.addToDirectory d) stack
        | Input.Cd   ".."  -> reduceTop2 (flip Entry.addToDirectory) stack
        | Input.Cd   path  -> Directory (path,[]) :: stack
        | Input.Dir  _
        | Input.Ls         -> stack

    List.fold inputToEntry [] ls
    |> List.reduce (flip Entry.addToDirectory)
    |> sortChildren

let treeToString t =
    let rec visitTree i (sb: System.Text.StringBuilder) e =
        let indentation = String.replicate i " "
        match e with
        | File (n,s) -> sb.AppendLine($"{indentation}- {n} (file, size={s})")
        | Directory(n,cs) -> sb.AppendLine($"{indentation}- {n} (dir)") |> ignore
                             List.iter (visitTree (i + 2) sb >> ignore) cs 
                             sb
    visitTree 0 (System.Text.StringBuilder()) t
    |> fun x -> x.ToString()

let folderSizes tree =
    let rec size e =
        match e with
        | File (n,s) -> false,n,s,[]
        | Directory (n,cs) -> let yee = List.map size cs
                              let folders = yee
                                            |> List.filter (fun (b,_,_,_) -> b)
                                            |> List.map (fun (_,n,s,fs) -> n,s,fs)
                              let totalSize = yee |> List.sumBy (fun (_,_,s,_) -> s)

                              let hmm  = List.collect (fun (_,_,fs) -> fs) folders

                              let thisFolder = n,totalSize
                              let toReturn = thisFolder :: hmm
                              
                              true,n,totalSize,toReturn

    size tree
    |> fun (_,_,_,fs) -> fs


let terminalInput = List.map parse input
let fileTree = terminalInput |> tree

let treeStr = fileTree |> treeToString

let fs = folderSizes fileTree

// We're not checking for sizes greater than 100000
// remember that the argument we're baking will be on the left
// and the values from the list will be on the right 
// fs |> List.map snd |> List.filter ((>=) 100000) |> List.sum 

let foldersDescending = fs |> List.sortByDescending snd

// File.WriteAllText("fattree.txt", treeStr)
let diskSize = 70000000
let neededForUpdate = 30000000
let usedSize = foldersDescending |> List.head |> snd
let unusedSize = diskSize - usedSize
let needToFree = neededForUpdate - unusedSize

// Find the smallest possible folder that
// frees at least needToFree
foldersDescending |> List.findBack (fun (_,s) -> s >= needToFree)
