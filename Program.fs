// TODO investigate using Argu for arg parsing
open Common

let usage = "USAGE: dotnet run [--help] [--puzzle <first|second>] <DAY> <INPUT>\n\
             \n\
             ARGUMENTS:\n\
             \n\
             <DAY>     The day of the advent calendar to select puzzles from.\n\
             <INPUT>   Path to a text file containing the input for the day.\n\
             \n\
             OPTIONS:\n\
             \n\
             --puzzle <first|second>    which of the two puzzles to solve. solves both if not specified.\n\
             --help                     display this list of options"

[<EntryPoint>]
let main args =
    if args.Length < 2 then
        printfn "%s" usage
        1
    else
        if args[0] = "--help" then
            printfn "%s" usage
        else
            let getPuzzle k v =
                if k = "--puzzle" then
                    match v with
                    | "first" -> Puzzle.First
                    | "second" -> Puzzle.Second
                    | _ -> Puzzle.None
                else
                    Puzzle.Both
            
            let puzzle = getPuzzle args[0] args[1]

            let required =
                if puzzle = Puzzle.Both then
                    Some (args[0],args[1])
                elif args.Length = 4 then
                    Some (args[2],args[3])
                else
                    None
            
            match required, puzzle with
            | _, Puzzle.None
            | None, _ -> printfn "%s" usage
            | Some (d, i), p -> match d with
                                | "1" -> CalorieCounting.solve i p
                                | "2" -> RockPaperScissors.solve i p
                                | "3" -> RucksackReorganization.solve i p
                                | "4" -> CampCleanup.solve i p
                                | "5" -> SupplyStacks.solve i p
                                | "6" -> TuningTrouble.solve i p
                                | "7" -> NoSpaceLeftOnDevice.solve i p
                                | _ -> failwith <| sprintf "No puzzles solved for day '%s'" d
        0
