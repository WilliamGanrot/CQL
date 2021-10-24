
open System.IO
open FParsec
open CQL.Parser
open CQL.Interpreter

[<EntryPoint>]
let main argv =

    let q = "1 + 3 * 5 - 2"
    match (run Parser.parithmetic q) with
    | Success(result, _, _)  ->
        printfn "Success: %A" (result)
        printfn ""
        match result with
        | ArithmeticExpression a->

            Interpreter.arithmeticExpression a |> printfn "%A" |> ignore
        | _ -> failwith ""
        //let o,e1,e2 = result

        //Interpreter.eval result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    0