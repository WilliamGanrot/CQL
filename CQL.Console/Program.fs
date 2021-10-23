
open System.IO
open FParsec
open CQL.Parser
open CQL.Interpreter

[<EntryPoint>]
let main argv =
    let input = "select '*' from 'person.txt'"
    match (run Parser.parse "  create   'new.csv'(select   '*' from 'people.csv' as 'v') as 'kart'") with
    | Success(result, _, _)  ->
        printfn "Success: %A" (result)
        //Interpreter.eval result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    0