module StateMonYetAgain

type State<'TCompute, 'TState> = State of ('TState -> 'TCompute * 'TState)

let runS s = 
    let (State func) = s
    func

let bindS f x =
    State (fun s0 -> 
        let a, s = runS x s0
        let s2 = f a
        runS s2 s)

let (>>=) x f = bindS f x

let returnS a = 
    State(fun s -> a, s)

[<Struct>]
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
    | SkipToLoopEnd of Loop

[<Struct>]
type WorldState = { DataPointer : int ; DataTape : byte[] ; InstructionPointer : int ; InstructionTape : string; LoopState : Loop}

let tapeLength = 1000_000
let initialState = {DataPointer = 0 ; DataTape = [|for _i = 1 to tapeLength do yield 0uy|]; InstructionPointer = 0; InstructionTape = "" ; LoopState = NoLoop}

let initInstructions instructions =
    { initialState with InstructionTape = instructions }

let getByteAtPtr state =
    state.DataTape.[state.DataPointer]

let incrementPointer () =
    State (fun s -> (), {s with DataPointer = s.DataPointer + 1 })

let decrementPointer () =
    State (fun s -> (), {s with DataPointer = s.DataPointer - 1 })

let incrementByte () =
    State (fun s -> 
        s.DataTape.[s.DataPointer] <- s.DataTape.[s.DataPointer] + 1uy
        (), s)

let decrementByte () =
    State (fun s -> 
        s.DataTape.[s.DataPointer] <- s.DataTape.[s.DataPointer] - 1uy
        (), s)

let outputByteAscii () =
    State( fun s ->
    printf "%c" ( char s.DataTape.[s.DataPointer] )
    (), s)

let outputByteNum () =
    State( fun s ->
    printf "%i" s.DataTape.[s.DataPointer]
    (), s )

let inputByte byte () =
    State( fun s -> 
    let newTape = s.DataTape
    newTape.[s.DataPointer] <- byte
    (), {s with DataTape = newTape})

let startLoop () =
    State(fun s ->
    let loopState = s.LoopState
    match loopState with
    | SkipToLoopEnd _ -> (), { s with LoopState = SkipToLoopEnd(s.LoopState)}
    | _ ->
        if getByteAtPtr s = 0uy then
            (), { s with LoopState = SkipToLoopEnd (s.LoopState) }
        else
            (), { s with LoopState = Loop (s.LoopState, s.InstructionPointer)}
     )   

let endLoop () =
    State(fun state ->
    let loop = state.LoopState
    match loop with
    | SkipToLoopEnd inner ->
        (), { state with LoopState = inner }
    | Loop (inner, loopStartIndex) ->
        if getByteAtPtr state = 0uy then
            (), { state with LoopState = inner }
        else 
            (), { state with InstructionPointer = loopStartIndex }
    | NoLoop -> failwith "Cannot end unstarted loop! (endLoop)"
    )

let incrInstructionPtr () =
    State(fun s ->
        (), { s with InstructionPointer = s.InstructionPointer + 1 }
    )

let instructionLookup = 
    let sequence = [
            ('>', IncrementPointer)         
            ('<', DecrementPointer)         
            ('+', IncrementData)            
            ('-', DecrementData)            
            ('.', Output)                   
            (',', Input (Input.takeInput))  
            ('[', LoopStart)                
            (']', LoopEnd)]
    dict sequence

let moveNextOneM () =
    State( fun state ->
        let instruction = instructionLookup.[state.InstructionTape.[state.InstructionPointer]]
        match state.LoopState with
        | SkipToLoopEnd _ -> 
            match instruction with
            | LoopStart -> runS (startLoop () >>= incrInstructionPtr) state
            | LoopEnd -> runS (endLoop () >>= incrInstructionPtr) state
            | _ ->  runS (incrInstructionPtr ()) state
        | _ -> 
            match instruction with
            | IncrementPointer -> runS (incrementPointer () >>= incrInstructionPtr) state
            | DecrementPointer -> runS (decrementPointer () >>= incrInstructionPtr) state
            | IncrementData -> runS (incrementByte       () >>= incrInstructionPtr) state
            | DecrementData -> runS (decrementByte       () >>= incrInstructionPtr) state
            | Input i ->       runS (inputByte (i())     () >>= incrInstructionPtr) state
            | Output ->        runS (outputByteAscii     () >>= incrInstructionPtr) state
            | LoopStart ->     runS (startLoop           () >>= incrInstructionPtr) state
            | LoopEnd ->       runS (endLoop             () >>= incrInstructionPtr) state
    )

//let executeToEnd state =
//        let infiniteSeq = Seq.init (1000) (fun _ -> moveNextOneM())
//
//        let initialState = () |> returnS
//        let executionChain = Seq.foldBack( fun _ state -> state >>= moveNextOneM) infiniteSeq initialState
//        runS executionChain state
//        
   
// kaip padaryt be mutable?
let executeToEnd state =
    let mutable currentWorldState = state
    let tapeLength = state.InstructionTape
    while currentWorldState.InstructionPointer <> String.length tapeLength do
        let (_, state) = runS (moveNextOneM()) currentWorldState
        currentWorldState <- state