module Parser
open State

let parseInctructions'' (instructionString : string)=
    let symbolList = instructionString.ToCharArray() |> List.ofArray
    let rec loop acc next =
        match next with
        | [] -> Ok (acc)
        | head::tail -> 
            match head with
            | '>' -> loop (acc @ [IncrementPointer]) tail
            | '<' -> loop (acc @ [DecrementPointer]) tail
            | '+' -> loop (acc @ [IncrementData]) tail
            | '-' -> loop (acc @ [DecrementData]) tail
            | '.' -> loop (acc @ [Output]) tail
            | ',' -> loop (acc @ [Input (Input.takeInput )]) tail
            | '[' -> loop (acc @ [LoopStart]) tail
            | ']' -> loop (acc @ [LoopEnd]) tail
            | c -> //Error (sprintf "Failed to parse '%c':" c) 
                loop acc tail
    loop [] symbolList

let parseInctructions' (instructionString : string) =
    let symbolList = instructionString.ToCharArray() |> List.ofArray
    let sysList = new System.Collections.Generic.List<Instruction>()
    let rec loop next =
        match next with
        | [] -> Ok sysList
        | head::tail -> 
            match head with
            | '>' -> sysList.Add(IncrementPointer) ; loop tail
            | '<' -> sysList.Add(DecrementPointer) ; loop tail
            | '+' -> sysList.Add(IncrementData) ; loop tail
            | '-' -> sysList.Add(DecrementData) ; loop tail
            | '.' -> sysList.Add(Output) ; loop tail
            | ',' -> sysList.Add(Input (Input.takeInput )) ; loop tail
            | '[' -> sysList.Add(LoopStart) ; loop tail
            | ']' -> sysList.Add(LoopEnd) ; loop tail
            | _ -> loop tail
    loop symbolList

let parseInctructions instructionList =
    let symbolList = instructionList
    let sysList = new System.Collections.Generic.List<Instruction>()
    let rec loop next =
        match next with
        | [] -> Ok sysList
        | head::tail -> 
            match head with
            | '>' -> sysList.Add(IncrementPointer) ; loop tail
            | '<' -> sysList.Add(DecrementPointer) ; loop tail
            | '+' -> sysList.Add(IncrementData) ; loop tail
            | '-' -> sysList.Add(DecrementData) ; loop tail
            | '.' -> sysList.Add(Output) ; loop tail
            | ',' -> sysList.Add(Input (Input.takeInput)) ; loop tail
            | '[' -> sysList.Add(LoopStart) ; loop tail
            | ']' -> sysList.Add(LoopEnd) ; loop tail
            | _ -> loop tail
    loop symbolList

let cleanInput (input : string) =
    let sequence = seq {
        for c in input do
            match c with
            | '>' | '<' | '+' | '-' 
            | '.' | ',' | '[' | ']' -> yield c
            | _ -> ()
        }
    List.ofSeq sequence