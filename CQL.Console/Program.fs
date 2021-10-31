
open System.IO
open FParsec
open CQL.Parser
open CQL.Interpreter

[<EntryPoint>]
let main argv =

    //let q = "select '*' from 'person.txt' as 'p' inner join 'adress.txt' as 'a' 'a.ownerid' + 'p.userid' + 'p.userid' = 'p.userid' * 2 + 'a.ownerid'"
    let q = "select '*' from 'person.txt' as 'p' left join 'adress.txt' as 'a' 'p.userid' = 'a.ownerid'" 
    match (run Parser.parse q) with
    | Success(result, _, _)  ->
        printfn "%A" result

        Interpreter.eval result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    0