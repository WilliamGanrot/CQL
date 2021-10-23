
open System.IO
open FParsec
open CQL.Parser
open CQL.Interpreter

[<EntryPoint>]
let main argv =
    //let input = "select '*' from 'person.txt'"
    //match (run Parser.parse "  create   'new.csv'(select   '*' from 'people.csv' as 'v') as 'kart'") with
    //| Success(result, _, _)  ->
    //    printfn "Success: %A" (result)
    //    //Interpreter.eval result
    //| Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    


    //let y = pcharCI (anyOf " \t\r\n") None
    //let s = (pstring "." >>. alphastring) |>> Some
    //let p = alphastring .>>. (s <|> (pstringCI (anyOf " \t\r\n") None)) |>> (fun (a,b) ->
    //    match a,b with
    //    | a,None -> (None,a)
    //    | a,b -> (b,a))

    //let charstilldotorspace isdotp isspacep = many1CharsTill anyChar ((anyOf ".") <|> (anyOf " \t\r\n" .))
    //let a = 
    //let afterdot = pstring "." >>. many1Chars anyChar
    //let space = spaces1

    //let charstilldot = manyCharsTill anyChar (afterdot <|> (spaces))
    //let parser = opt(charstilldot) .>>. (many1Chars anyChar)

    //let z = (charstilldotorspace .>>. (pstring "." >>. many1Chars anyChar) .>>? many1Chars anyChar) .>> spaces


    //let withAlias = (((many1CharsTill anyChar (pstring ".") |>> Some) .>>. (many1Chars anyChar)) |>> Specifict)
    //let noAlias = (many1Chars anyChar |>> (fun s -> (Specifict(None,s))))
    //let x = all <|> withAlias <|> noAlias

    match (run Parser.queryType "select    'name',   'adress' from 'people.csv'  where 'id' = 3 where 'name' = \"bert\"") with
    | Success(result, _, _)  ->
        printfn "Success: %A" (result)
        //Interpreter.eval result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    0