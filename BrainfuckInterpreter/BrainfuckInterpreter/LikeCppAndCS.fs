module LikeCppAndCS

open System.Collections.Generic

let private tapeLenght = 1000_000
type Interpreter (instructionTape) =
    let instTapeLength = String.length instructionTape

    let mutable DataPointer = 0
    let mutable InstructionPointer = 0
    let LoopState = new Stack<int>()
    let DataTape = [|for _i in 1..tapeLenght -> 0uy|]
    let InstructionTape = instructionTape
    let GetByteAtCurrentPtr() = DataTape.[DataPointer]
    let SetByteAtCurrentPtr(value) = DataTape.[DataPointer] <- value

    let IncrementPointer() =
        DataPointer <- DataPointer + 1
    let DecrementPointer() =
        DataPointer <- DataPointer - 1
    let IncrementData() =
        SetByteAtCurrentPtr(GetByteAtCurrentPtr() + 1uy)
    let DecrementData() =
        SetByteAtCurrentPtr(GetByteAtCurrentPtr() - 1uy)
    let Output() =
        printf "%c" (char (GetByteAtCurrentPtr()))
    let Input() =
        let input = byte (System.Console.ReadKey().KeyChar)
        SetByteAtCurrentPtr(input)
    let StartLoop() =
        if(LoopState.Count <> 0 && LoopState.Peek() < 0) then
            LoopState.Push(-1)
        else if(GetByteAtCurrentPtr() > 0uy) then
            LoopState.Push(InstructionPointer)
        else if(GetByteAtCurrentPtr() = 0uy) then
            LoopState.Push(-1)
    let EndLoop() =
        if(LoopState.Count = 0) then failwith "Negalima baigti nepradėto loopo!!"
        else if(LoopState.Peek() < 0 ) then
            LoopState.Pop() |> ignore
        else if(LoopState.Peek() >= 0) then
            if(GetByteAtCurrentPtr() > 0uy) then
                InstructionPointer <- LoopState.Peek()
            else LoopState.Pop() |> ignore

    let MoveNextInstruction() =
        let nextInstructionChar = instructionTape.[InstructionPointer]
        if(nextInstructionChar <> '[' && nextInstructionChar <> ']' && LoopState.Count > 0 && LoopState.Peek() < 0) then
            InstructionPointer <- InstructionPointer + 1
        else
            match nextInstructionChar with
            | '>' -> IncrementPointer()
            | '<' -> DecrementPointer()
            | '+' -> IncrementData()
            | '-' -> DecrementData()
            | '.' -> Output()
            | ',' -> Input()
            | '[' -> StartLoop()
            | ']' -> EndLoop()
            | _ -> ()
            InstructionPointer <- InstructionPointer + 1

    member this.ExecuteToEnd() =
        while InstructionPointer <> instTapeLength do
            MoveNextInstruction()
      