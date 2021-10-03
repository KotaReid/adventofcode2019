open System

let executeDay n =
    match n with
    | "1" -> Day1.run()
    | "2" -> Day2.run()
    | _ -> Console.WriteLine "Invalid Option"

let rec promptUser () =
    try
        Console.Write "Enter Number of Day to Execute [q to quit]: "

        let input = Console.ReadLine().Trim()

        match input with
        | "q" -> 0
        | _ ->
            executeDay input
            Console.WriteLine()
            promptUser ()
    with
    | ex ->
        Console.WriteLine(ex.ToString())
        1

[<EntryPoint>]
let main argv = promptUser()
