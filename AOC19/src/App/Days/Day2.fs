module Day2

open System.IO

(*
let i = [|1L;0L;0L;0L;99L|]
let c = new Computer(i);;
*)

type ComputerRunResult =
    | Success
    | InvalidOp of int64

type Computer(instructions: int64 array) = 
    let memory = Array.copy instructions

    let readMemory pos = memory.[(int)memory.[pos]] 
    
    let setMemory pos value = memory.[(int)memory.[pos]] <- value

    let fourInstructionOp pos (op: int64 -> int64 -> int64) =
        let readPos1 = readMemory (pos + 1) 
        let readPos2 = readMemory (pos + 2)
        setMemory (pos + 3) (op readPos1 readPos2)
        pos + 4

    let add pos = fourInstructionOp pos (+)

    let multiply pos = fourInstructionOp pos ( * )

    let rec run pos = 
        let instruction = memory.[pos]
        match instruction with
        | 1L -> add pos |> run 
        | 2L -> multiply pos |> run
        | 99L -> Success
        | _ -> InvalidOp instruction

    member this.Run() = run 0

    member this.Set pos value = memory.[pos] <- value

    member this.Get pos = memory.[pos] 

let getInstructions (filePath: string) =
    let codes = File.ReadLines(filePath) |> Seq.head
    codes.Split ',' |> Array.map int64

let part1 (instructions: int64 array) = 
    let computer = Computer instructions
    computer.Set 1 12L
    computer.Set 2 2L
    
    match computer.Run() with 
    | Success -> computer.Get 0 |> printfn "Part 1 %i"
    | InvalidOp error -> printfn "Part 1 encountered an invalid opcode %i" error

let rec part2 (instructions: int64 array) = 
    let solve noun verb = 
        let computer = Computer instructions
        computer.Set 1 noun
        computer.Set 2 verb
        match computer.Run() with
        | Success -> computer.Get 0
        | InvalidOp e -> -1L
    
    seq {
        for noun = 0L to 99L do
            for verb = 0L to 99L do
                if solve noun verb = 19690720L then
                    100L * noun + verb } |> Seq.head |> printfn "Part 2 %i" 

let run() =
    let instructions = getInstructions "Files/day2.txt"
    part1 instructions
    part2 instructions