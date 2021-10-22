namespace CQL.Interpreter
open CQL.Table
open CQL.Parser
open System.IO

module Interpreter =

    let eval query =
    
        let rec evalSelectQuery (selectQuery:SelectQuery) = 
            let (cols,from,joins,where) = selectQuery
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
            | Select(cols,from,joins,where) -> SelectQuery(cols,from,joins,where) |> evalSelectQuery |> Table.printTable
            | Create(name, select) ->
                evalSelectQuery select |> Table.saveTableAsCsv name |> ignore
                printfn "table %A saved" name |> ignore
    
        evalQuery query
            

