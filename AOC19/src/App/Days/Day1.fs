module Day1

open System
open System.IO

let readLines filePath = File.ReadLines(filePath)

let run =
    let lines = readLines "Files/day1.txt"
    lines |> Seq.iter (fun t -> Console.WriteLine(t))
