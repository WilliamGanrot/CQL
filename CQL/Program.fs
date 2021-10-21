open System.IO
open CQL.Table
open CQL.Parser
open FParsec

let eval query =

    let rec evalSelectQuery (selectQuery:SelectQuery) = 
        let (cols,from,where) = selectQuery |> fun (x,y,z) -> (x,y,z)
        let table = getTable from
        Table.getSubTable table cols

    and getTable from =
        match from with
        | TableName filename -> 
            let lines = File.ReadAllLines filename |> Array.toList
            let columns = lines.Head.Split [|';'|] |> Array.toList
            let contentRows = lines.Tail |> List.map (fun row -> row.Split [|';'|] |> Array.toList)
            Table.create None columns contentRows
        | SubQuery select -> evalSelectQuery select

    let evalQuery query =
        match query with
        | Select(cols,from,where) -> SelectQuery(cols,from,where) |> evalSelectQuery |> Table.printTable
        | Create(name, select) ->
            evalSelectQuery select |> Table.saveTableAsCsv name |> ignore
            printfn "table %A saved" name |> ignore

    evalQuery query
        
[<EntryPoint>]
let main argv =

    let input = "select '*' from 'person.txt'"
    match (run Parser.queryType input) with
    | Success(result, _, _)  ->
        printfn "Success: %A" (result)
        eval result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg



    //let table = {alias = None; 
    //             headers = ["name";"age";"adress"]; 
    //             contentRows = [["william";"24"; "flöjtgatan"];["fatima";"22";"flöjtgaran"];["bengt";"45";"skoghall"]]}



    //let t2 = Create ("name.txt",
    //                 ([Specifict "name"],
    //                  SubQuery ([Specifict "name"; Specifict "age"],
    //                            SubQuery ([All], TableName "person.txt", []), []), []))

    //eval t2

    printfn ""
    //Table.getSubTable table [Specifict("name")] |> ignore     
    0