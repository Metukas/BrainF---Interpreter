module StateImperative

open State'

type StateImperative (instructionTape) =
    let dataTapeLength = String.length instructionTape
    let DataTape : byte[] = [|for i in 1..dataTapeLength -> 0uy|]
    let InstructionTape : string = instructionTape
    member val InstructionLength = InstructionTape.Length
    member val DataPointer = 0 with get, set
    member val InstructionPointer = 0 with get, set
    member val LoopState : Loop = NoLoop with get, set
    member this.IncrementDataPtr() = this.DataPointer <- this.DataPointer + 1
    member this.DecrementDataPtr() = this.DataPointer <- this.DataPointer - 1
    member this.IncrementData() = DataTape.[this.DataPointer] <-DataTape.[this.DataPointer] + 1uy
    member this.DecrementData() = DataTape.[this.DataPointer] <- DataTape.[this.DataPointer] - 1uy

    member this.IncrementInstructionPtr() = this.InstructionPointer <- this.InstructionPointer + 1
    member this.DecrementInstructionPtr() = this.InstructionPointer <- this.InstructionPointer - 1
    member this.OutputByteAscii() = printf "%c" (char DataTape.[this.DataPointer])
    member this.Input i = DataTape.[this.DataPointer] <- i()
    member this.StartLoop() =
        match this.LoopState with
        | SkipToLoopEnd _ -> this.LoopState <- SkipToLoopEnd(this.LoopState)
        | _ ->
            if DataTape.[this.DataPointer] = 0uy then 
                this.LoopState <- SkipToLoopEnd(this.LoopState)  
            else
                this.LoopState <- Loop (this.LoopState, this.InstructionPointer)   
    member this.EndLoop()=
        match this.LoopState with
        | SkipToLoopEnd inner ->
            this.LoopState <- inner
        | Loop (inner, loopStartIndex) ->
            if DataTape.[this.DataPointer] = 0uy then
                this.LoopState <- inner
            else 
                this.InstructionPointer <- loopStartIndex
        | NoLoop -> failwith "Cannot end unstarted loop! (endLoop)"
    member this.MoveOne () =
        let instruction = instructionLookup.[InstructionTape.[this.InstructionPointer]]
        match this.LoopState with
        | SkipToLoopEnd _ -> 
            match instruction with
            | LoopStart -> this.StartLoop(); this.IncrementInstructionPtr()
            | LoopEnd -> this.EndLoop() ; this.IncrementInstructionPtr()
            | _ -> this.IncrementInstructionPtr()
        | _ -> 
            match instruction with
            | IncrementPointer -> this.IncrementDataPtr(); this.IncrementInstructionPtr()
            | DecrementPointer -> this.DecrementDataPtr(); this.IncrementInstructionPtr()
            | IncrementData -> this.IncrementData()      ; this.IncrementInstructionPtr()
            | DecrementData -> this.DecrementData()      ; this.IncrementInstructionPtr()
            | Input i -> this.Input i                    ; this.IncrementInstructionPtr()
            | Output -> this.OutputByteAscii()           ; this.IncrementInstructionPtr()
            | LoopStart -> this.StartLoop()              ; this.IncrementInstructionPtr()
            | LoopEnd -> this.EndLoop()                  ; this.IncrementInstructionPtr()
    member this.ExecuteToEnd() =
        while this.InstructionLength <> this.InstructionPointer  do
            this.MoveOne()