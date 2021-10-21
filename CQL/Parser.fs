namespace CQL.Parser

[<AutoOpen>]
module ParserDomain =

    type EqualityOperator =
        | Equal
        | NotEqual

    type ComparisonExpression =
        | ColumnExpression of string //uncouted string
        | NumberExpression of float
        | StringExpression of string //quoted string
        | Binary of EqualityOperator * ComparisonExpression * ComparisonExpression

    type Column =
        | Specifict of string
        | All

    type From =
        | TableName of string
        | SubQuery of SelectQuery

    and SelectQuery = Column List * From * ComparisonExpression list
    and CreateQuery = string * SelectQuery

    and Query =
        | Select of SelectQuery
        | Create of CreateQuery

module Parser =
    open FParsec

    let queryType, queryTypeRef = createParserForwardedToRef<Query, unit>()
    let fromType, fromTypeRef = createParserForwardedToRef<From, unit>()
    
    let opp = OperatorPrecedenceParser<ComparisonExpression, _, _>()
    let equalityExpression = opp.ExpressionParser

    let manyCharsBetween popen pclose pchar = popen >>? manyCharsTill pchar pclose
    let anyStringBetween popen pclose = manyCharsBetween popen pclose anyChar
    let anyStringBetweenStrings s1 s2 = anyStringBetween (pstring s1) (pstring s2)
    let singleQoutedString = anyStringBetweenStrings "'" "'"
    let doubleQoutedString = anyStringBetweenStrings "\"" "\""

    let intlitteral = pint32 .>> spaces |>> (fun i -> (float i)) |>> NumberExpression
    let floatlitteral : Parser<_,unit> = pfloat .>> spaces |>> NumberExpression

    let numberExpression = floatlitteral <|> intlitteral 
    let stringExpression = doubleQoutedString |>> StringExpression .>> spaces
    let columnExpression = singleQoutedString |>> ColumnExpression .>> spaces

    let where = pstring "where" >>. spaces >>. equalityExpression .>> spaces
    let selectParameter = 
        singleQoutedString
        |>> (fun x ->
                match x with
                | "*" -> All
                | name -> Specifict(name))
    let manySelectParameter = 
        let seperator = pstring "," .>> spaces 
        sepBy1 selectParameter seperator
    let create = spaces >>. pstring "create" >>. spaces1 >>. singleQoutedString .>> spaces 
    let select = spaces >>. pstring "select" >>. spaces1 >>. manySelectParameter .>> spaces
    //let inSelect = spaces >>. pstring "select" >>. spaces1 >>. selectParameter .>> spaces //can only have one select parameter

    let from fromCase = pstring "from" .>> spaces >>. fromCase .>> spaces
    let selectQuery = select .>>. (spaces >>. from fromType) .>>. many where |>> fun((x,y),z) -> SelectQuery(x,y,z)
    let subSelectQuery = (between (pstring "(") (pstring ")") selectQuery)
    let createQuery = create.>>. subSelectQuery .>> spaces |>> fun (x,y) -> CreateQuery(x,y)


    do queryTypeRef := choice [selectQuery |>> Select
                               createQuery |>> Create]

    do fromTypeRef := choice [singleQoutedString |>> TableName
                              subSelectQuery |>> SubQuery]

    opp.TermParser <- choice [numberExpression;stringExpression;columnExpression]
    opp.AddOperator <| InfixOperator("=", spaces, 1, Associativity.None, fun x y -> Binary(Equal, x, y))
    opp.AddOperator <| InfixOperator("<>", spaces, 1, Associativity.None, fun x y -> Binary(NotEqual, x, y))

