
namespace CQL.Parser

[<AutoOpen>]
module ParserDomain =

    type EqualityOperator =
        | Equal
        | NotEqual

    type SpecificColumn = string Option * string
    type ComparisonExpression =
        | ColumnExpression of SpecificColumn //uncouted string
        | NumberExpression of float
        | StringExpression of string //quoted string
        | Binary of EqualityOperator * ComparisonExpression * ComparisonExpression

    
    type Column =
        | Specifict of SpecificColumn
        | All

    type From =
        | TableName of name: string * alias: string Option
        | SubQuery of subQuery: SelectQuery * alias: string Option

     and InnerJoin = From * ComparisonExpression list
     and LeftJoin = From * ComparisonExpression list
     and Join =
         | Inner of InnerJoin
         | Left of LeftJoin

    and SelectQuery = Column List * From * Join list * ComparisonExpression list
    and CreateQuery = string * From

    and Query =
        | Select of SelectQuery
        | Create of CreateQuery

module Parser =
    open FParsec

    let keywords =
        ["select"
         "from"
         "in"
         "as"
         "create"
         "left join"
         "inner join"]

    //let pKeyword =
    //   (many1Satisfy isLower .>> nonAlphaNumeric) // [a-z]+
    //   >>= (fun s -> if keyWordSet.Contains(s) then (preturn x) else fail "not a keyword")

    let queryType, queryTypeRef = createParserForwardedToRef<Query, unit>()
    let fromType, fromTypeRef = createParserForwardedToRef<From, unit>()
    let joinType, joinTypeRef = createParserForwardedToRef<Join, unit>()

    let opp = OperatorPrecedenceParser<ComparisonExpression, _, _>()
    let equalityExpression = opp.ExpressionParser
    let manyEqualityExpression = 
        let seperator = pstring "&&" .>> spaces 
        sepBy1 equalityExpression seperator

    let alphastring : Parser<_,unit> = many1Chars (anyOf "abcdefghijklmnopqrstuvwxyz")
    let manyCharsBetween popen pclose pchar = popen >>? manyCharsTill pchar pclose
    let anyStringBetween popen pclose = manyCharsBetween popen pclose anyChar
    let anyStringBetweenStrings s1 s2 = anyStringBetween (pstring s1) (pstring s2)
    let singleQoutedString = anyStringBetweenStrings "'" "'"
    let doubleQoutedString = anyStringBetweenStrings "\"" "\""
    let betweenString s1 s2 p = between (pstring s1) (pstring s2) p

    let intlitteral = pint32 .>> spaces |>> (fun i -> (float i)) |>> NumberExpression
    let floatlitteral = pfloat .>> spaces |>> NumberExpression

    let all = stringReturn "*" All
    let specificColumn =
        let colTargetWithoutDot = spaces |>> fun _ -> None
        let colTargetWithDot = (pstring "." >>. alphastring) |>> Some
        alphastring .>>. (colTargetWithDot <|> colTargetWithoutDot) |>> (fun (h,t) ->
            match h,t with
            | h,None -> (None,h) |> SpecificColumn
            | h,Some(t) -> (Some(h),t) |> SpecificColumn)

    let numberExpression = floatlitteral <|> intlitteral 
    let stringExpression = doubleQoutedString |>> StringExpression .>> spaces
    let columnExpression = betweenString "'" "'" specificColumn |>> ColumnExpression .>> spaces

    let selectColumns = between (pstring "'") (pstring "'") (all <|> (specificColumn |>> Specifict))

    let where = pstring "where" >>. spaces >>. equalityExpression .>> spaces
    let manySelectParameter = 
        let seperator = pstring "," .>> spaces 
        sepBy1 selectColumns seperator

    let create = spaces >>. pstring "create" >>. spaces1 >>. singleQoutedString .>> spaces
    let select = spaces >>. pstring "select" >>. spaces1 >>. manySelectParameter .>> spaces1

    let innerjoin = pstring "inner join" >>. spaces1 >>. fromType .>> spaces .>>. manyEqualityExpression .>> spaces |>> InnerJoin
    let leftjoin = pstring "left join" >>. spaces1 >>. fromType .>> spaces .>>. manyEqualityExpression .>> spaces |>> LeftJoin

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
                              leftjoin |>> Left]


    opp.TermParser <- choice [numberExpression;stringExpression;columnExpression]
    opp.AddOperator <| InfixOperator("=", spaces, 1, Associativity.None, fun x y -> Binary(Equal, x, y))
    opp.AddOperator <| InfixOperator("<>", spaces, 1, Associativity.None, fun x y -> Binary(NotEqual, x, y))

    let parse = spaces >>. queryType