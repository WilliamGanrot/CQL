namespace CQL.Interpreter
open CQL.Table
open CQL.Parser
open System.IO
open System.Globalization
open System

module Interpreter =

    let (|StringIsInt|_|) (str:string) =
        match System.Int32.TryParse str with
        | true,int -> Some int
        | _ -> None

    let (|StringIsFloat|_|) (str:string) =

        let stringFloatWithComma = str.Replace(".",",")
        match System.Double.TryParse stringFloatWithComma with
        | true, f -> Some (float f)
        | z-> None

    let (|StringIsFalse|_|) (str:string) =
        match str = "false" || str = "FALSE" with
        | true -> Some 0
        | _ -> None

    let (|StringIsTrue|_|) (str:string) =
        match str = "true" || str = "TRUE" with
        | true -> Some 1
        | _ -> None

    let (|StringIsNull|_|) (str:string) =
        match str = "null" || str = "NULL" with
        | true -> Some str
        | _ -> None

    let (|StringIsQouted|_|) (str:string) =
        match str.[0] = '"' && str.[str.Length-1] = '"' || str.[0] = ''' && str.[str.Length-1] = ''' with
        | true -> Some str
        | _ -> None

    let guessDataTypeOfStringContent = function
        | StringIsQouted s -> StringLitteral s
        | StringIsFalse i -> BoolLitteral i
        | StringIsTrue i -> BoolLitteral i
        | StringIsNull _ -> NullLitteral
        | StringIsFloat f -> NumericLitteral f
        | StringIsInt s -> NumericLitteral (float s)
        | s -> StringLitteral s

    let litteralIsTrue = function
        | BoolLitteral l when l = 0 -> false
        | _ -> true

    let rec getLitteralFromExpression aexpr =
        match aexpr with
        | LitteralExpression.Litteral l -> l
        | LitteralExpression.ArithmeticExpression (opp, a1, a2) ->
            let a1' = getLitteralFromExpression a1
            let a2' = getLitteralFromExpression a2

            match (opp, a1', a2') with
            | Add, NumericLitteral(i1), NumericLitteral(i2) -> NumericLitteral(i1 + i2)
            | Subtract, NumericLitteral(i1), NumericLitteral(i2) -> NumericLitteral(i1 - i2)
            | Divide, NumericLitteral(i1), NumericLitteral(i2) -> NumericLitteral(i1 / i2)
            | Multiply, NumericLitteral(i1), NumericLitteral(i2) -> NumericLitteral(i1 * i2)

            | Add, BoolLitteral b1, BoolLitteral b2 -> BoolLitteral(b1 + b2)
            | Subtract, BoolLitteral b1, BoolLitteral b2 -> BoolLitteral(b1 - b2)
            | Divide, BoolLitteral b1, BoolLitteral b2 -> BoolLitteral(b1 / b2)
            | Multiply, BoolLitteral b1, BoolLitteral b2 -> BoolLitteral(b1 * b2)

            | Add, BoolLitteral b1, NumericLitteral n2 -> BoolLitteral(b1 + (int n2))
            | Subtract, BoolLitteral b1, NumericLitteral n2 -> BoolLitteral(b1 - (int n2))
            | Divide, BoolLitteral b1, NumericLitteral n2 -> BoolLitteral(b1 / (int n2))
            | Multiply, BoolLitteral b1, NumericLitteral n2 -> BoolLitteral(b1 * (int n2))

            | Add, NumericLitteral n1, BoolLitteral b2 -> BoolLitteral((int n1) + b2)
            | Subtract, NumericLitteral n1, BoolLitteral b2 -> BoolLitteral((int n1) - b2)
            | Divide, NumericLitteral n1, BoolLitteral b2 -> BoolLitteral((int n1) / b2)
            | Multiply, NumericLitteral n1, BoolLitteral b2 -> BoolLitteral((int n1) * b2)

            | Add, StringLitteral s1, StringLitteral s2 -> StringLitteral(s1 + s2)

            | _ -> BoolLitteral(0)

        | LitteralExpression.EqualityExpression (opp, a1, a2) ->
            let l1 = getLitteralFromExpression a1
            let l2 = getLitteralFromExpression a2

            match (opp, l1, l2) with
            | Equals, l1, l2 -> if l1 = l2 then BoolLitteral 1 else BoolLitteral 0
            | NotEquals, l1, l2 -> if l1 <> l2 then BoolLitteral 1 else BoolLitteral 0
            | GreaterThan, NumericLitteral(l1), NumericLitteral(l2) -> if l1 > l2 then BoolLitteral 1 else BoolLitteral 0
            | GreaterThan,_,_ -> failwith "> is not a valid operator for this type"

            | GreaterThanOrEquals, NumericLitteral(l1), NumericLitteral(l2) -> if l1 >= l2 then BoolLitteral 1 else BoolLitteral 0
            | GreaterThanOrEquals,_,_ -> failwith ">= is not a valid operator for this type"

            | LesserThan, NumericLitteral(l1), NumericLitteral(l2) -> if l1 < l2 then BoolLitteral 1 else BoolLitteral 0
            | LesserThan,_,_ -> failwith "< is not a valid operator for this type"

            | LesserThanOrEquals, NumericLitteral(l1), NumericLitteral(l2) -> if l1 <= l2 then BoolLitteral 1 else BoolLitteral 0
            | LesserThanOrEquals,_,_ -> failwith "<= is not a valid operator for this type"

    let rec queryExpressionToLitteralExpression expr (row:string list) headers =
        match expr with
        | QueryExpression.ColumnIdentifier ci ->
            row.[Table.getheaderIndex headers ci]
            |> guessDataTypeOfStringContent
            |> LitteralExpression.Litteral

        | QueryExpression.Litteral l -> LitteralExpression.Litteral (l)

        | QueryExpression.ArithmeticExpression (opp, q1, q2) ->
            let q1' = queryExpressionToLitteralExpression q1 row headers
            let q2' = queryExpressionToLitteralExpression q2 row headers
            LitteralExpression.ArithmeticExpression (opp, q1', q2')

        | QueryExpression.EqualityExpression (opp, q1, q2) ->
            let q1' = queryExpressionToLitteralExpression q1 row headers
            let q2' = queryExpressionToLitteralExpression q2 row headers
            LitteralExpression.EqualityExpression (opp, q1', q2')

    let maybeOrder order table =
        match order with
        | None -> table
        | Some (field, direction) ->

            let headerIndex = Table.getheaderIndex table.Headers field
            let litteralColumnToSort = table.ContentRows |> List.mapi(fun i row -> i, row.[headerIndex])

            let orderdIndexes = 
                match direction with
                | Ascending -> litteralColumnToSort |> List.sortBy(fun (_, l) -> l)
                | Decending -> litteralColumnToSort |> List.sortByDescending(fun (_, l) -> l)

            orderdIndexes
            |> List.map(fun (i,_) -> table.ContentRows.[i])
            |> Table.create None table.Headers

    let maybeTop top table =
        match top with
        | None -> table
        | Some (Top top) -> Table.create None table.Headers table.ContentRows.[0..top-1]

    let rec evalSelectQuery (selectQuery: SelectQuery) = 
        let cols, from, joins, wheres, order, top = selectQuery

        getTable from
        |> joinTables joins
        |> evaluateWheres wheres
        |> maybeOrder order
        |> maybeTop top
        |> Table.getSelectColumns cols

    and evaluateWheres wheres table =
        match wheres with
        | [] -> table
        | Where queryExpression :: t ->
            let table' =
                table.ContentRows
                |> List.choose (fun row ->
                    let litteral =
                        queryExpressionToLitteralExpression queryExpression row table.Headers
                        |> getLitteralFromExpression
                    if litteralIsTrue litteral then Some row else None)
                |> Table.create None table.Headers
            evaluateWheres t table'

    and fulljoin originTable tableToJoin =
        let mergedHeaders = originTable.Headers @ tableToJoin.Headers

        let newTableRows =
            [for originRow in originTable.ContentRows do
                for rowToJoin in tableToJoin.ContentRows do
                    originRow @ rowToJoin]

        Table.create None mergedHeaders newTableRows

    and joinTables joins table =
        match joins with
        | [] -> table
        | Full from :: t ->
            let fullyjoined = getTable from |> fulljoin table
            joinTables t fullyjoined
        | Inner(from,expr) :: t ->
            let fullyJoinedTable = getTable from |> fulljoin table

            let innerjoined =
                fullyJoinedTable.ContentRows
                |> List.choose(fun row ->

                    let litteral =
                        queryExpressionToLitteralExpression expr row fullyJoinedTable.Headers
                        |> getLitteralFromExpression

                    if litteralIsTrue litteral then Some row else None )
                |> Table.create None fullyJoinedTable.Headers

            joinTables t innerjoined
        | Left _ :: t -> failwith "not implemented"

    and getTable from : Tabel =
        match from with
        | TableName (filename, alias) -> 
            let lines = File.ReadAllLines filename |> Array.toList
            let columns = lines.Head.Split [|';'|] |> Array.toList |> List.map (fun name -> (None,name) |> SpecificColumn)
            let contentRows = lines.Tail |> List.map (fun row -> row.Split [|';'|] |> Array.toList)
            Table.create alias columns contentRows
        | SubQuery (select,alias) ->
            let table = evalSelectQuery select
            let newheaders = table.Headers |> List.map (fun (_,h) -> (alias,h) |> SpecificColumn)
            {table with Headers = newheaders; Alias = alias}

    let eval query =
        match query with
        | Select(cols,from,joins,where, order,top) -> SelectQuery(cols,from,joins,where,order,top) |> evalSelectQuery |> Table.printTable
        | Create(name, from) ->
            getTable from |> Table.saveTableAsCsv name |> ignore
            printfn "table %A saved" name |> ignore

            

