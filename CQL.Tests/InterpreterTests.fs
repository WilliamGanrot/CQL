
module InterpreterTests
open CQL.Interpreter
open System
open Xunit
open CQL.Parser
open CQL.Table
open FParsec

[<Fact>]
let ``translate queryExpression to LitteralExpression without any columspecific field`` () =
    let queryExpresion : QueryExpression =
        (EqualityExpression
            (GreaterThan,
             ArithmeticExpression
               (Add, Litteral (NumericLitteral 8.0),
                Litteral (NumericLitteral 2.0)),
             ArithmeticExpression
               (Divide,
                ArithmeticExpression
                  (Multiply, Litteral (NumericLitteral 1.0),
                   Litteral (NumericLitteral 10.0)),
                Litteral (NumericLitteral 2.0))))

    let litteralExpresion : LitteralExpression =
        (LitteralExpression.EqualityExpression
            (GreaterThan,
             LitteralExpression.ArithmeticExpression
               (Add, LitteralExpression.Litteral (NumericLitteral 8.0),
                LitteralExpression.Litteral (NumericLitteral 2.0)),
             LitteralExpression.ArithmeticExpression
               (Divide,
                LitteralExpression.ArithmeticExpression
                  (Multiply, LitteralExpression.Litteral (NumericLitteral 1.0),
                   LitteralExpression.Litteral (NumericLitteral 10.0)),
                LitteralExpression.Litteral (NumericLitteral 2.0))))

    let result = Interpreter.queryExpressionToLitteralExpression queryExpresion [""] [(None, "")]

    result = litteralExpresion |> Assert.True

[<Theory>]
[<InlineData("\"fweoufweg\"")>]
[<InlineData("\"fweouf weg\"")>]
[<InlineData("\"1234\"")>]
[<InlineData("\"123f4\"")>]
[<InlineData("\"123f 4\"")>]
[<InlineData("\"12.34 \"")>]
[<InlineData(" .12 34 ")>]
[<InlineData(" 12. 34 ")>]
[<InlineData("\"true\"")>]
[<InlineData("\"TRUE\"")>]
[<InlineData("\"false\"")>]
[<InlineData("\"FALSE\"")>]

[<InlineData("'fweoufweg'")>]
[<InlineData("'fweouf weg'")>]
[<InlineData("'1234'")>]
[<InlineData("'123f4'")>]
[<InlineData("'12.34 '")>]
[<InlineData("'true'")>]
[<InlineData("'TRUE'")>]
[<InlineData("'false'")>]
[<InlineData("'FALSE'")>]
let ``guessDataTypeOfStringContent when expecting to return stringlitteral`` (s) =
    let litteral = Interpreter.guessDataTypeOfStringContent s
    match litteral with
    | StringLitteral _ -> Assert.True true
    | _ -> Assert.True false


[<Theory>]
[<InlineData("FALSE")>]
[<InlineData("false")>]
let ``guessDataTypeOfStringContent when expecting to return Boollitteral false`` (s) =
    let litteral = Interpreter.guessDataTypeOfStringContent s
    match litteral with
    | BoolLitteral b -> Assert.Equal(0, b)
    | _ -> Assert.True false

[<Theory>]
[<InlineData("TRUE")>]
[<InlineData("true")>]
let ``guessDataTypeOfStringContent when expecting to return Boollitteral true`` (s) =
    let litteral = Interpreter.guessDataTypeOfStringContent s
    match litteral with
    | BoolLitteral b -> Assert.Equal(1, b)
    | _ -> Assert.True false


[<Theory>]
[<InlineData("2.0", 2.0)>]
[<InlineData("2342.4589",2342.4589)>]
[<InlineData("2.456 ",2.456)>]
[<InlineData("0.1231",0.1231)>]
[<InlineData("0.1",0.1)>]
[<InlineData(".1",0.1)>]
[<InlineData(",1",0.1)>]
[<InlineData("2,0", 2.0)>]
[<InlineData("2342,4589",2342.4589)>]
[<InlineData("2,456 ",2.456)>]
[<InlineData("0,1231",0.1231)>]
[<InlineData("0,1",0.1)>]
let ``guessDataTypeOfStringContent when expecting to return NumericLitteral of float`` (s,e) =
    let litteral = Interpreter.guessDataTypeOfStringContent s
    match litteral with
    | Litteral.NumericLitteral b -> Assert.Equal (e, b)
    | _ -> Assert.True false


[<Theory>]
[<InlineData("2", 2)>]
[<InlineData("2342", 2342)>]
[<InlineData("456", 456)>]
[<InlineData("0",0)>]
let ``guessDataTypeOfStringContent when expecting to return NumericLitteral of int`` (s, e) =
    let litteral = Interpreter.guessDataTypeOfStringContent s
    match litteral with
    | Litteral.NumericLitteral b -> Assert.Equal(e, (float b))
    | _ -> Assert.True false