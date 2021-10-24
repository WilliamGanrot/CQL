
open System.IO
open FParsec
open CQL.Parser
open CQL.Interpreter

[<EntryPoint>]
let main argv =

    let q = "select '*' from 'person.txt' as 'p' full join 'adress.txt'"
    match (run Parser.queryType q) with
    | Success(result, _, _)  ->
        printfn "Success: %A" (result)
        printfn ""
        Interpreter.eval result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    0