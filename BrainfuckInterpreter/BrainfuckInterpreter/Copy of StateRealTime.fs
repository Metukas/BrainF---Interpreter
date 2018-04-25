module State''
open System
let tapeLenght = 1000_000

type Instruction =
    | IncrementPointer
    | DecrementPointer
    | IncrementData
    | DecrementData
    | Input of (unit -> byte)
    | Output
    | LoopStart
    | LoopEnd

type Loop = 
    | NoLoop 
    | Loop of Loop * loopStartInstructionIndex : int

type State = { DataPointer : int ; DataTape : byte[] ; InstructionPointer : int ; InstructionTape : string; LoopState : Loop}

let initialState = {DataPointer = 0 ; DataTape = [|for _i = 1 to tapeLenght do yield 0uy|]; InstructionPointer = 0; InstructionTape = "" ; LoopState = NoLoop}

let initInstructions instructions =
    { initialState with InstructionTape = instructions }

let getByteAtPtr state =
    state.DataTape.[state.DataPointer]

let changeState state f  =
    f state

let incrementPointer state =
    let func = fun s -> {s with DataPointer = s.DataPointer + 1 }
    changeState state func

let decrementPointer state =
    let func = fun s -> {s with DataPointer = s.DataPointer - 1 }
    changeState state func

let incrementByte state =
    let newTape = state.DataTape
    newTape.[state.DataPointer] <- newTape.[state.DataPointer] + 1uy
    { state with DataTape = newTape }

let decrementByte state =
    let newTape = state.DataTape
    newTape.[state.DataPointer] <- newTape.[state.DataPointer] - 1uy
    { state with DataTape = newTape }

let outputByteAscii state =
    printf "%c" ( char state.DataTape.[state.DataPointer] )
    state
let outputByteNum state =
    printf "%i" state.DataTape.[state.DataPointer]
    state

let inputByte byte state =
    let newTape = state.DataTape
    newTape.[state.DataPointer] <- byte
    {state with DataTape = newTape}

let startLoop state =
    { state with LoopState = Loop (state.LoopState, state.InstructionPointer) }    

let endLoop state =
    let loop = state.LoopState
    match loop with 
    | Loop (inner, loopStartIndex) ->
        if getByteAtPtr state = 0uy then
            { state with LoopState = inner }
        else 
            { state with InstructionPointer = loopStartIndex }
    | NoLoop -> failwith "Cannot end unstarted loop! (endLoop)"

let incrInstructionPtr state =
    { state with InstructionPointer = state.InstructionPointer + 1 }

let parseInstruction instructionChar =
    match instructionChar with
    | '>' -> IncrementPointer
    | '<' -> DecrementPointer
    | '+' -> IncrementData
    | '-' -> DecrementData
    | '.' -> Output
    | ',' -> Input (Input.takeInput)
    | '[' -> LoopStart
    | ']' -> LoopEnd
    | c -> failwith (sprintf "%c is not valid brainfuck instruction" c)

let moveOne state =
    match parseInstruction(state.InstructionTape.[state.InstructionPointer]) with
    | IncrementPointer -> incrementPointer state |> incrInstructionPtr  
    | DecrementPointer -> decrementPointer state |> incrInstructionPtr
    | IncrementData -> incrementByte state       |> incrInstructionPtr
    | DecrementData -> decrementByte state       |> incrInstructionPtr
    | Input i -> inputByte (i()) state               |> incrInstructionPtr
    | Output -> outputByteAscii state            |> incrInstructionPtr
    | LoopStart -> startLoop state               |> incrInstructionPtr
    | LoopEnd -> endLoop state                   |> incrInstructionPtr

let moveN n state =
    let mutable currentState = state
    for i in [1..n] do 
        currentState <- currentState |> moveOne

let moveInfinite state =
    let mutable currentState = state
    let infiniteSeq = Seq.initInfinite (fun _ -> currentState <- currentState |> moveOne)
    try
        Seq.iter id infiniteSeq
    with 
        | :? Exception as ex -> printfn "Done: %s" ex.Message