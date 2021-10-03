module Day2

open System.IO

let getInstructions (filePath: string) =
    let codes = File.ReadLines(filePath) |> Seq.head
    codes.Split ',' |> Array.map int64

type ComputerResult =
    | Success of int64
    | InvalidOp of int64

let readMemory (instructions: int64 array) (pos: int64) =
    instructions.[(int) pos]

let setMemory (instructions: int64 array) (pos: int64) (value: int64) =
    instructions.[(int) pos] <- value

let fourInstructionOp (instructions: int64 array) (pos: int64) (op: int64 -> int64 -> int64) =
    let readPos1 = readMemory instructions (pos + 1L) |> readMemory instructions
    let readPos2 = readMemory instructions (pos + 2L) |> readMemory instructions
    let setPostion = readMemory instructions (pos + 3L)
    setMemory instructions setPostion (op readPos1 readPos2)
    pos + 4L

//Opcode 1
let add (instructions: int64 array) (pos: int64) = 
    fourInstructionOp instructions pos (+)

//Opcode 2
let multiply (instructions: int64 array) (pos: int64) =
    fourInstructionOp instructions pos (*)

let rec computer (instructions: int64 array) (answerPos: int64) (pos: int64) =
    let instruction = readMemory instructions pos
    match instruction with
    | 1L -> add instructions pos |> computer instructions answerPos
    | 2L -> multiply instructions pos |> computer instructions answerPos
    | 99L -> readMemory instructions answerPos |> Success
    | _ -> InvalidOp instruction

let part1 (instructions: int64 array) = 
    instructions.[1] <- 12L
    instructions.[2] <- 2L 
    match computer instructions 0L 0L with 
    | Success result -> printfn "Part 1 %i" result
    | InvalidOp error -> printfn "Part 1 encountered an invalid opcode %i" error

let rec part2 (instructions: int64 array) = 
    let solve noun verb = 
        let instructionsCopy = Array.copy instructions
        instructionsCopy.[1] <- noun
        instructionsCopy.[2] <- verb
        match computer instructionsCopy 0L 0L with
        | Success result -> result
        | InvalidOp e -> -1L
    
    seq {
        for noun = 0L to 99L do
            for verb = 0L to 99L do
                if solve noun verb = 19690720L then
                    100L * noun + verb } |> Seq.head |> printfn "Part 2 %i" 

let run =
    let instructions = getInstructions "Files/day2.txt"
    Array.copy instructions |> part1
    Array.copy instructions |> part2