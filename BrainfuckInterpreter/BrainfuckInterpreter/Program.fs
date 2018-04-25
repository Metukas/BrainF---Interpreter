open State'
open System
open Parser
open System.IO
open StateImperative
open StateMonYetAgain

[<EntryPoint>]
let main argv = 
    
    let cleanInput (input : string) =
        let sequence = seq {
            for c in input do
                match c with
                | '>' | '<' | '+' | '-' 
                | '.' | ',' | '[' | ']' -> yield c
                | _ -> ()
            }
        new String( Array.ofSeq sequence )

    let codeFile = File.ReadAllText("brainfuck.b")
    let clean = cleanInput codeFile

    //let state = initInstructions clean

    //State'.moveInfinite state
    
    let steitas = StateImperative.StateImperative(clean) // imperative
    let wtfState = StateMonYetAgain.initInstructions clean // state monad
    let wtfStateMut = StateMonadMutable.initInstructions clean // mutable record state monad
    printfn "begin"

    //StateMonYetAgain.executeToEnd wtfState
    //StateMonadMutable.executeToEnd wtfStateMut
    //steitas.ExecuteToEnd()
    let intrp = new LikeCppAndCS.Interpreter(clean) // lik cpp and cs
    intrp.ExecuteToEnd()

    0 // return an integer exit code
