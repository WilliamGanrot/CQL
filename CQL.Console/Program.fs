
open System.IO
open FParsec
open CQL.Parser
open CQL.Interpreter

[<EntryPoint>]
let main argv =

    let q = "(2 + (2+3))"
    match (run Parser.expr q) with
    | Success(result, _, _)  ->
        printfn "Success: %A" (result)
        //Interpreter.eval result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    0