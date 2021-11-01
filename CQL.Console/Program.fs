
open FParsec
open CQL.Parser
open CQL.Interpreter

[<EntryPoint>]
let main argv =

    let args = argv |> Array.toList |> List.tail

    let query =
        args
        |> List.head
        |> fun s -> if (s.[0] = '"' && s.[s.Length-1] = '"') then s.[1..s.Length-1] else s

    match (run Parser.parse query) with
    | Success(result, _, _) -> Interpreter.eval result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    0