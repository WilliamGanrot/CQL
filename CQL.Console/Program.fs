
open System.IO
open FParsec
open CQL.Parser
open CQL.Interpreter

[<EntryPoint>]
let main argv =

    let q = "select 'p.age' from (select '*' from 'person.txt') as 'p'"
    match (run Parser.queryType "select '*' from 'person.txt' where \"x\"") with
    | Success(result, _, _)  ->
        printfn "Success: %A" (result)
        Interpreter.eval result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    0