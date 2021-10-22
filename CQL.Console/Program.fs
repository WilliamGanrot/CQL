
open System.IO
open FParsec
open CQL.Parser
open CQL.Interpreter

[<EntryPoint>]
let main argv =
    let input = "select '*' from 'person.txt'"
    match (run Parser.queryType "select '*' from ( select '*' from (select 'name' from 'table') )") with
    | Success(result, _, _)  ->
        printfn "Success: %A" (result)
        //Interpreter.eval result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    0