
namespace CQL.Parser

[<AutoOpen>]
module ParserDomain =

    type BinaryExprKind =
        | Add
        | Subtract
        | Multiply
        | Divide
        | And
        | Or
        | Equals
        | NotEquals
        | GreaterThan
        | GreaterThanOrEquals
        | LesserThan
        | LesserThanOrEquals


    type Litteral =
        | NumericLitteral of float
        | StringLitteral of string
        | BoolLitteral of int

    type SpecificColumn = string Option * string
    type Expression =
        | ColumnExpression of SpecificColumn
        | LitteralExpresion of Litteral
        | Binary of BinaryExprKind * Expression * Expression

    type Column =
        | Specifict of SpecificColumn
        | All

    type From =
        | TableName of name: string * alias: string Option
        | SubQuery of subQuery: SelectQuery * alias: string Option

     and InnerJoin = From * Expression list
     and LeftJoin = From * Expression list
     and FullJoin = From 
     and Join =
         | Inner of InnerJoin
         | Left of LeftJoin
         | Full of FullJoin

    and SelectQuery = Column List * From * Join list * Expression list
    and CreateQuery = string * From

    and Query =
        | Select of SelectQuery
        | Create of CreateQuery


module Parser =
    open FParsec

    let alphabet = "abcdefghijklmnopqrstuvwxyz"
    let keywords =
        ["select"
         "from"
         "in"
         "as"
         "create"
         "left join"
         "inner join"]
    
    let queryType, queryTypeRef = createParserForwardedToRef<Query, unit>()
    let fromType, fromTypeRef = createParserForwardedToRef<From, unit>()
    let joinType, joinTypeRef = createParserForwardedToRef<Join, unit>()

    let opp = OperatorPrecedenceParser<Expression, _, _>()
    let expr : Parser<_,unit> = opp.ExpressionParser

    let alphastring : Parser<_,unit> = many1Chars (anyOf alphabet)
    let manyCharsBetween popen pclose pchar = popen >>? manyCharsTill pchar pclose
    let anyStringBetween popen pclose = manyCharsBetween popen pclose anyChar
    let anyStringBetweenStrings s1 s2 = anyStringBetween (pstring s1) (pstring s2)
    let singleQoutedString = anyStringBetweenStrings "'" "'"
    let doubleQoutedString : Parser<_,unit> = anyStringBetweenStrings "\"" "\""
    let betweenString s1 s2 p = between (pstring s1) (pstring s2) p

    let equal : Parser<_,unit> = stringReturn "=" Equals
    let notequal : Parser<_,unit> = stringReturn "<>" NotEquals

    let all = stringReturn "*" All
    let specificColumn = opt(attempt (manyCharsTill (anyOf alphabet) (pstring "."))) .>>. alphastring

    let intlitteral : Parser<_,unit> = pint32 .>> spaces |>> fun x -> (float x) |> NumericLitteral
    let floatlitteral = pfloat .>> spaces |>> NumericLitteral

    let numberExpression = (floatlitteral <|> intlitteral) |>> LitteralExpresion
    let stringExpression = doubleQoutedString |>> StringLitteral .>> spaces |>> LitteralExpresion
    let columnExpression = betweenString "'" "'" specificColumn |>> ColumnExpression .>> spaces
    let boolExpression : Parser<_,unit> = ((stringReturn "true" (BoolLitteral 1)) <|> (stringReturn "false" (BoolLitteral 0))) |>> LitteralExpresion
 
    let manyEqualityExpression = 
        let seperator = pstring "&&" .>> spaces 
        sepBy1 expr seperator

    let selectColumns = between (pstring "'") (pstring "'") (all <|> (specificColumn |>> Specifict))
    let where = pstring "where" >>. spaces >>. expr .>> spaces
    let manySelectParameter = 
        let seperator = pstring "," .>> spaces 
        sepBy1 selectColumns seperator

    let create = spaces >>. pstring "create" >>. spaces1 >>. singleQoutedString .>> spaces
    let select = spaces >>. pstring "select" >>. spaces1 >>. manySelectParameter .>> spaces1

    let innerjoin = pstring "inner join" >>. spaces1 >>. fromType .>> spaces .>>. manyEqualityExpression .>> spaces |>> InnerJoin
    let leftjoin = pstring "left join" >>. spaces1 >>. fromType .>> spaces .>>. manyEqualityExpression .>> spaces |>> LeftJoin
    let fulljoin = pstring "full join" >>. spaces1 >>. fromType .>> spaces

    let joins = opt(many1 joinType) |>> (fun joinlist ->
        match joinlist with
        | Some l -> l
        | None -> [])

    let wheres = opt (many1 (where)) |>> (fun wherelist ->
        match wherelist with
        | Some l -> l
        | None -> [])

    let alias = opt(pstring "as" >>. spaces1 >>. singleQoutedString)
    let from = pstring "from" .>> spaces1 >>. fromType .>> spaces 
    let tableName = singleQoutedString .>>. (spaces >>. alias)

    let selectQuery = select .>>. from .>>. joins .>>. wheres .>> spaces |>> fun(((x,y),z),a) -> SelectQuery(x,y,z,a)
    let selectQueryWitheof = selectQuery .>> eof 
    let subSelectQuery = ((between (pstring "(") (pstring ")") (selectQuery .>> spaces)) .>> spaces) .>>. alias

    let createQuery = create .>>. fromType .>> spaces |>> fun (filename,(from)) -> CreateQuery(filename,from)



    do queryTypeRef := choice [selectQueryWitheof |>> Select
                               createQuery |>> Create]

    do fromTypeRef := choice [tableName |>> TableName
                              subSelectQuery |>> SubQuery]

    do joinTypeRef := choice [innerjoin |>> Inner
                              leftjoin |>> Left
                              fulljoin |>> Full]


    opp.AddOperator <| InfixOperator("=", spaces, 1, Associativity.None, fun x y -> Binary (Equals,x, y))
    opp.AddOperator <| InfixOperator("<>", spaces, 2, Associativity.None, fun x y -> Binary (NotEquals, x, y))
    opp.AddOperator <| InfixOperator(">", spaces, 3, Associativity.None, fun x y -> Binary (GreaterThan, x, y))
    opp.AddOperator <| InfixOperator(">=", spaces, 4, Associativity.None, fun x y -> Binary (GreaterThanOrEquals, x, y))
    opp.AddOperator <| InfixOperator("<", spaces, 5, Associativity.None, fun x y -> Binary (LesserThan, x, y))
    opp.AddOperator <| InfixOperator("<=", spaces, 6, Associativity.None, fun x y -> Binary (LesserThanOrEquals, x, y))
    opp.AddOperator <| InfixOperator("+", spaces, 7, Associativity.Left, fun x y -> Binary (Add, x, y))
    opp.AddOperator <| InfixOperator("*", spaces, 8, Associativity.Left, fun x y -> Binary (Multiply, x, y))
    opp.AddOperator <| InfixOperator("/", spaces, 9, Associativity.Left, fun x y -> Binary (Divide, x, y))

    let expressionLittetrals  = choice [numberExpression;
                                       stringExpression;
                                       columnExpression;
                                       boolExpression;]
    opp.TermParser <- (expressionLittetrals  .>> spaces) <|> between (spaces >>. pstring "(" >>. spaces) (spaces >>. pstring ")" >>. spaces) expr


    let parse = spaces >>. queryType