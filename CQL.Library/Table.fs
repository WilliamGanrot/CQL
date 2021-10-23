namespace CQL.Table
open CQL.Parser
open System.IO

[<AutoOpen>]
module TableDomain =
    type Tabel = {
        headers: SpecificColumn list
        contentRows: string list list
        alias: string Option }

module Table =

    let create alias headers rows =
        {alias = alias;
         headers = headers |> List.map(fun s -> (SpecificColumn (alias,s)));
         contentRows = rows}

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
                | Specifict (alias,name) -> (SpecificColumn (alias,name))
                | All(_)-> yield! table.headers ]

        let rows =
            [for row in table.contentRows do
                [for col in cols do
                    match col with
                    | All(_) -> yield! row
                    | Specifict (alias,name) -> 
                        let index = getheaderIndex table (SpecificColumn (alias,name))
                        row.[index] ]]
                 
        {alias = None; contentRows = rows; headers = stringcols}

    let tableToCsvRows table =
        let s = table.headers |> List.map (fun (a,n) -> n)
        let headerString = (String.concat ";" s)
        let contentRows = table.contentRows |> List.map (String.concat ";")
        headerString :: contentRows

    let printTable table =
        tableToCsvRows table
        |> List.iter(fun r -> printfn "%A" r)

    let saveTableAsCsv name table =
        let rows = tableToCsvRows table
        File.WriteAllLines(name, rows) 