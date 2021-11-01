namespace CQL.Table
open CQL.Parser
open System.IO

[<AutoOpen>]
module TableDomain =
    type Tabel = {
        Headers: SpecificColumn list
        ContentRows: string list list
        Alias: string Option }

module Table =

    let create alias headers rows =

        {Alias = alias;
         Headers =
            match alias with
            | None -> headers
            | Some a -> headers |> List.map (fun(_,name) -> (alias,name) |> SpecificColumn);
         ContentRows = rows}

    let getheaderIndex headers (colToFind:SpecificColumn) =

        let validateFoundMatchCols name foundColumnsWithIndexes =
            match foundColumnsWithIndexes with
            | [] -> failwith ("could not find column " + name)
            | [(index, _)] -> index
            | _ -> failwith ("ambigious column name " + name)

        let indexedHeaders = headers |> List.indexed
        match colToFind with
        | ((Some alias), name) ->
            indexedHeaders
            |> List.filter (fun (_,(a,n)) -> a = Some(alias) && n = name)
            |> validateFoundMatchCols (alias + "." + name)
        | None, name ->
            indexedHeaders
            |> List.filter (fun (_,(_,n)) -> n = name)
            |> validateFoundMatchCols (name)

    let getSelectColumns (cols: Column List) table =

        let headers =
            [for col in cols do
               match col with
                | Specifict (alias,name) -> (SpecificColumn (alias,name))
                | All -> yield! table.Headers ]

        let rows =
            [for row in table.ContentRows do
                [for col in cols do
                    match col with
                    | All(_) -> yield! row
                    | Specifict (alias,name) -> 
                        let index = getheaderIndex table.Headers (SpecificColumn (alias,name))
                        row.[index] ]]
                 
        {Alias = table.Alias; ContentRows = rows; Headers = headers}

    let tableToCsvRows table =
        let s = table.Headers |> List.map (fun (a,n) -> n)
        let headerString = (String.concat ";" s)
        let contentRows = table.ContentRows |> List.map (String.concat ";")
        headerString :: contentRows

    let printTable table =

        tableToCsvRows table
        |> List.iter(fun r ->
            r.Split [|';'|] |> Array.iter (fun cell -> printf "%10s" cell)
            printfn "")

    let saveTableAsCsv name table =
        let rows = tableToCsvRows table
        File.WriteAllLines(name, rows)
        printfn "table %A saved" name |> ignore