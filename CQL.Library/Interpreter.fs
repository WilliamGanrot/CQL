namespace CQL.Interpreter
open CQL.Table
open CQL.Parser
open System.IO

module Interpreter =

    let rec equalityComparison opp litteral1 litteral2 =
        match opp, litteral1, litteral2 with
        | Equals, l1, l2 -> l1 = l2
        | NotEquals, l1, l2 -> l1 <> l2
        | GreaterThan, NumericLitteral(l1), NumericLitteral(l2) -> l1 > l2
        | GreaterThan,_,_ -> failwith "> is not a valid operator for this type"
        | GreaterThanOrEquals, NumericLitteral(l1), NumericLitteral(l2) -> l1 >= l2
        | GreaterThanOrEquals,_,_ -> failwith ">= is not a valid operator for this type"
        | LesserThan, NumericLitteral(l1), NumericLitteral(l2) -> l1 < l2
        | LesserThan,_,_ -> failwith "< is not a valid operator for this type"
        | LesserThanOrEquals, NumericLitteral(l1), NumericLitteral(l2) -> l1 <= l2
        | LesserThanOrEquals,_,_ -> failwith "<= is not a valid operator for this type"
        //| Add, NumericLitteral(l1), NumericLitteral(l2) -> i1 + i2
        //| Subtract, NumericLitteral(l1), NumericLitteral(l2) -> i1 -i2


    let rec evalSelectQuery (selectQuery:SelectQuery) = 
        let (cols,from,joins,where) = selectQuery

        let table =
            getTable from
            |> tryJoinTables joins

        Table.getSelectColumns table cols

    and fulljoin originTable tableToJoin =
        let mergedHeaders = originTable.headers @ tableToJoin.headers

        let newTableRows =
            [for originRow in originTable.contentRows do
                for rowToJoin in tableToJoin.contentRows do
                    originRow @ rowToJoin]

        Table.create None mergedHeaders newTableRows

    and tryJoinTables (joins: Join list) originTable =
        match joins with
        | [] -> originTable
        | Full(from) :: t -> getTable from |> fulljoin originTable
        | Inner(from,expressions) :: t ->
            let tableToJoin = getTable from
            let fullyJoinedTable = fulljoin originTable tableToJoin

            let expr = expressions.Head //only handles one expression AND need to recursivly compute all additions
            
            fullyJoinedTable.contentRows
            |> List.choose(fun row ->
                match expr with
                | EqualityExpression(op, ColumnExpression name1, ColumnExpression name2) ->

                    let v1 = row.[Table.getheaderIndex fullyJoinedTable name1]
                    let v2 = row.[Table.getheaderIndex fullyJoinedTable name2]

                    if equalityComparison op (StringLitteral v1) (StringLitteral v2) then Some row else None

                | EqualityExpression(op, ColumnExpression name, (NumericLitteral(i))) ->
                    let tableV = row.[Table.getheaderIndex fullyJoinedTable name] |> float
                    if equalityComparison op (NumericLitteral tableV) (NumericLitteral i) then Some row else None

                | EqualityExpression(op, (NumericLitteral(i)), ColumnExpression name) ->
                    let tableV = row.[Table.getheaderIndex fullyJoinedTable name] |> float
                    if equalityComparison op (NumericLitteral i) (NumericLitteral tableV) then Some row else None

                | EqualityExpression(op, (StringLitteral s), ColumnExpression name) ->
                    let tableV = row.[Table.getheaderIndex fullyJoinedTable name]
                    if (equalityComparison op (StringLitteral s) (StringLitteral tableV)) || (equalityComparison Equals (StringLitteral  ("\"" + s + "\"")) (StringLitteral tableV)) then Some row else None 

                | EqualityExpression(op, ColumnExpression name, (StringLitteral s)) ->
                    let tableV = row.[Table.getheaderIndex fullyJoinedTable name]
                    if (equalityComparison op (StringLitteral tableV) (StringLitteral s)) || (equalityComparison Equals (StringLitteral  ("\"" + s + "\"")) (StringLitteral tableV)) then Some row else None

                | EqualityExpression(op,  (BoolLitteral b), ColumnExpression name) ->
                    let tableV = row.[Table.getheaderIndex fullyJoinedTable name]
                    let tablebool = if tableV = "true" then (BoolLitteral 1) else (BoolLitteral 0)
                    if (equalityComparison op (BoolLitteral b) tablebool) then Some row else None

                | EqualityExpression(op, ColumnExpression name, (BoolLitteral b)) ->
                    let tableV = row.[Table.getheaderIndex fullyJoinedTable name]
                    let tablebool = if tableV = "true" then (BoolLitteral 1) else (BoolLitteral 0)
                    if (equalityComparison op tablebool (BoolLitteral b)) then Some row else None

                | (BoolLitteral(i)) when i = 0 -> None 
                | _ -> Some row )
            |> Table.create None fullyJoinedTable.headers



    and getTable from : Tabel =
        match from with
        | TableName (filename, alias) -> 
            let lines = File.ReadAllLines filename |> Array.toList
            let columns = lines.Head.Split [|';'|] |> Array.toList |> List.map (fun name -> (None,name) |> SpecificColumn)
            let contentRows = lines.Tail |> List.map (fun row -> row.Split [|';'|] |> Array.toList)
            Table.create alias columns contentRows
        | SubQuery (select,alias) ->
            let table = evalSelectQuery select
            let newheaders = table.headers |> List.map (fun (_,h) -> (alias,h) |> SpecificColumn)
            {table with headers = newheaders; alias = alias}

    let eval query =
        match query with
        | Select(cols,from,joins,where) -> SelectQuery(cols,from,joins,where) |> evalSelectQuery |> Table.printTable
        | Create(name, from) ->
            getTable from |> Table.saveTableAsCsv name |> ignore
            printfn "table %A saved" name |> ignore

            

