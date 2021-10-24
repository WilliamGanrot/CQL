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
         headers =
            match alias with
            | None -> headers
            | Some a -> headers |> List.map (fun(_,name) -> (alias,name) |> SpecificColumn);
         contentRows = rows}

    let getheaderIndex table colToFind =

        let indexedHeaders = table.headers |> List.indexed

        let validateFoundMatchCols name foundColumnsWithIndexes =
            match foundColumnsWithIndexes with
            | [] -> failwith ("could not find column " + name)
            | (index, column)::[] -> index
            | _ -> failwith ("ambigious column name " + name)

        match colToFind with
        | (Some alias), name ->
            indexedHeaders
            |> List.filter (fun (i,(a,n)) -> a = Some(alias) && n = name)
            |> validateFoundMatchCols (alias + "." + name)
        | None, name ->
            indexedHeaders
            |> List.filter (fun (i,(a,n)) -> n = name)
            |> validateFoundMatchCols (name)

    let getSelectColumns table (cols: Column List) =

        let headers =
            [for col in cols do
               match col with
                | Specifict (alias,name) -> (SpecificColumn (alias,name))
                | All -> yield! table.headers ]

        let rows =
            [for row in table.contentRows do
                [for col in cols do
                    match col with
                    | All(_) -> yield! row
                    | Specifict (alias,name) -> 
                        let index = getheaderIndex table (SpecificColumn (alias,name))
                        row.[index] ]]
                 
        {alias = table.alias; contentRows = rows; headers = headers}

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