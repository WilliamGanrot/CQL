namespace CQL.Table
open CQL.Parser
open System.IO

[<AutoOpen>]
module TableDomain =
    type Tabel = {
        headers: string list
        contentRows: string list list
        alias: string Option }

module Table =

    let create alias headers rows = {alias = alias; headers = headers; contentRows = rows}

    let getheaderIndex table columnName =
        table.headers 
        |> List.indexed
        |> List.filter(fun (_, v) -> v = columnName)
        |> List.map(fun(i,_) -> i)
        |> List.head

    let getSubTable table (cols: Column List) =
        
        let stringcols =
            [for col in cols do
               match col with
                | Specifict name -> name
                | All -> yield! table.headers ]

        let rows =
            [for row in table.contentRows do
                [for col in cols do
                    match col with
                    | All -> yield! row
                    | Specifict name -> 
                        let index = getheaderIndex table name
                        row.[index] ]]
                 
        {alias = None; contentRows = rows; headers = stringcols}

    let tableToCsvRows table =
        let headerString = (String.concat ";" table.headers)
        let contentRows = table.contentRows |> List.map (String.concat ";")
        headerString :: contentRows

    let printTable table =
        tableToCsvRows table
        |> List.iter(fun r -> printfn "%A" r)

    let saveTableAsCsv name table =
        let rows = tableToCsvRows table
        File.WriteAllLines(name, rows) 