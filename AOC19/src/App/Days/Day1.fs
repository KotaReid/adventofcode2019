module Day1

open System.IO

let getModules (filePath: string) = 
    File.ReadLines(filePath) 
    |> Seq.map int

let requiredFuel mass = mass / 3 - 2

let requiredFuel2 (mass: int) =
    let rec reqFuel mass =
        if mass <= 0 then 0
        else mass + (reqFuel (requiredFuel mass))
    requiredFuel mass |> reqFuel

let part1 input = 
    Seq.sumBy requiredFuel input
    |> printfn "Part 1 %i"

let part2 input =
    Seq.sumBy requiredFuel2 input
    |> printfn "Part 2 %i"

let run() =
    let input = getModules "Files/day1.txt" 
    part1 input
    part2 input
