
namespace CQL.Parser

[<AutoOpen>]
module ParserDomain =

    type ArithmeticOpperator = Add | Subtract | Multiply | Divide
    type EqualityOpperator = Equals | NotEquals | GreaterThan | GreaterThanOrEquals | LesserThan | LesserThanOrEquals

    type SpecificColumn = string Option * string

    type Expression =
        | ColumnExpression of SpecificColumn
        | NumericLitteral of float
        | StringLitteral of string
        | BoolLitteral of int
        | ArithmeticExpression of Arithmetic
        | EqualityExpression of Equality

    and Arithmetic = ArithmeticOpperator * Expression * Expression
    and Equality = EqualityOpperator * Expression * Expression


    type Column =
        | Specifict of SpecificColumn
        | All

    type From =
        | TableName of name: string * alias: string Option
        | SubQuery of subQuery: SelectQuery * alias: string Option

     and InnerJoin = From * Expression list
     and LeftJoin = From * Expression list
     and Join =
         | Inner of InnerJoin
         | Left of LeftJoin
         | Full of From

    and SelectQuery = Column List * From * Join list * Expression list
    and CreateQuery = string * From

    type Query =
        | Select of SelectQuery
        | Create of CreateQuery


module Parser =
    open FParsec

    let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIHKLMNOPQRSTUVWXYZ"
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

    let oppa = OperatorPrecedenceParser<Expression,_,_>()
    let oppc = OperatorPrecedenceParser<Expression, _, _>()

    let parithmetic = oppa.ExpressionParser
    let pcomparison = oppc.ExpressionParser

    let alphastring : Parser<_,unit> = many1Chars (anyOf alphabet)
    let manyCharsBetween popen pclose pchar = popen >>? manyCharsTill pchar pclose
    let anyStringBetween popen pclose = manyCharsBetween popen pclose anyChar
    let anyStringBetweenStrings s1 s2 = anyStringBetween (pstring s1) (pstring s2)
    let singleQoutedString = anyStringBetweenStrings "'" "'"
    let doubleQoutedString : Parser<_,unit> = anyStringBetweenStrings "\"" "\""
    let betweenString s1 s2 p = between (pstring s1) (pstring s2) p

    let all = stringReturn "*" All
    let specificColumn = opt(attempt (manyCharsTill (anyOf alphabet) (pstring "."))) .>>. alphastring

    let intlitteral : Parser<_,unit> = pint32 .>> spaces |>> fun x -> (float x)
    let floatlitteral = pfloat .>> spaces

    let numberExpression = (floatlitteral <|> intlitteral) |>> NumericLitteral
    let stringExpression = doubleQoutedString .>> spaces |>> StringLitteral
    let columnExpression = betweenString "'" "'" specificColumn |>> ColumnExpression .>> spaces
    let boolExpression = ((stringReturn "true" (BoolLitteral 1)) <|> (stringReturn "false" (BoolLitteral 0)))
 
    let manyEqualityExpression = 
        let seperator = pstring "&&" .>> spaces 
        sepBy1 pcomparison seperator

    let selectColumns = between (pstring "'") (pstring "'") (all <|> (specificColumn |>> Specifict))
    let where = pstring "where" >>. spaces >>. pcomparison .>> spaces
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

    let expressionLittetrals  = choice [numberExpression;
                                       stringExpression;
                                       columnExpression;
                                       boolExpression;]

    oppa.TermParser <- spaces >>. expressionLittetrals .>> spaces
    oppa.AddOperator(InfixOperator("+", spaces, 1, Associativity.Left, fun x y -> ArithmeticExpression(Add, x, y)))
    oppa.AddOperator(InfixOperator("-", spaces, 1, Associativity.Left, fun x y -> ArithmeticExpression(Subtract, x, y)))
    oppa.AddOperator(InfixOperator("*", spaces, 2, Associativity.Left, fun x y -> ArithmeticExpression(Multiply, x,y)))
    oppa.AddOperator(InfixOperator("/", spaces, 2, Associativity.Left, fun x y -> ArithmeticExpression(Divide, x,y)))
    

    oppc.TermParser <- spaces >>. parithmetic .>> spaces
    oppc.AddOperator(InfixOperator("=", spaces, 1, Associativity.Left, fun x y -> EqualityExpression(Equals, x, y)))
    oppc.AddOperator(InfixOperator("<>", spaces, 1, Associativity.Left, fun x y -> EqualityExpression(NotEquals, x, y)))
    oppc.AddOperator(InfixOperator("<=", spaces, 2, Associativity.Left, fun x y -> EqualityExpression(LesserThanOrEquals, x, y)))
    oppc.AddOperator(InfixOperator(">=", spaces, 2, Associativity.Left, fun x y -> EqualityExpression(GreaterThanOrEquals, x, y)))
    oppc.AddOperator(InfixOperator("<", spaces, 2, Associativity.Left, fun x y -> EqualityExpression(LesserThan, x, y)))
    oppc.AddOperator(InfixOperator(">", spaces, 2, Associativity.Left, fun x y -> EqualityExpression(GreaterThan, x, y)))

    let parse = spaces >>. queryType