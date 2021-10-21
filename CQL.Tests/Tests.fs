module Tests

open System
open Xunit
open CQL.Parser
open CQL.Table
open FParsec

[<Theory>]
[<InlineData("select    'name',   'adress' from 'people.csv'  where 'id' = 3 where 'name' = \"bert\"")>]
[<InlineData("select 'name', 'adress' from 'people.csv' where 'id' = 3")>]
[<InlineData(" select 'name',  'adress'  from 'people.csv  '")>]
[<InlineData("select '*' from 'people.csv' where 'id' = 3 where 'name' = \"bert\"")>]
[<InlineData("select '*'  from 'people.csv'   where 'id' = 3")>]
[<InlineData("select '*' from 'people.csv'")>]
[<InlineData("select '*' from (select '*' from 'name.csv')")>]
[<InlineData("select '*' from ( select 'name', 'age' from 'name.csv' where 'age' = 20)")>]
let ``select query happy cases`` (query) =

    match (run Parser.selectQuery query) with
    | Success(result, _, _)  -> Assert.True true
    | Failure(errorMsg, _, _) -> Assert.True false
    Assert.True(true)

[<Theory>]
[<InlineData("select , 'adress' from 'people.csv' where 'id' = 3 where 'name' = \"bert\"")>]
[<InlineData("select name, 'adress' from 'people.csv' where 'id' = 3")>]
[<InlineData("select from 'people.csv'")>]
[<InlineData("selectfrom 'people.csv' where 'id' = 3 where 'name' = \"bert\"")>]
[<InlineData("select '*' from people.csv where 'id' = 3")>]
[<InlineData("select '*' from 'people.csv' whered 'id' = 2")>]
[<InlineData("select '*' from whered 'id' = 2")>]
[<InlineData("select '*' from (select '*' from 'name.csv'")>]
[<InlineData("select '*' from select '*' from 'name.csv'")>]
[<InlineData("select '*' from select '*' from ")>]
let ``select query unhappy cases`` (query) =

    match (run Parser.selectQuery query) with
    | Success(result, _, _)  -> Assert.True false
    | Failure(errorMsg, _, _) -> Assert.True true
    Assert.True(true)

[<Theory>]
[<InlineData("create 'new.csv' (select 'name', 'adress' from 'people.csv' where 'id' = 3 where 'name' = \"bert\"  )  ")>]
[<InlineData("create 'new.csv' (  select '*' from 'people.csv')")>]
[<InlineData("  create   'new.csv'(select   '*' from 'people.csv')")>]
let ``create query happy cases`` (query) =
    
    match (run Parser.createQuery query) with
    | Success(result, _, _)  -> Assert.True true
    | Failure(errorMsg, _, _) -> Assert.True false
    Assert.True(true)


[<Theory>]
[<InlineData("create  (select 'name', 'adress' from 'people.csv' where 'id' = 3 where 'name' = \"bert\")")>]
[<InlineData("create'new.csv' (select '*' from 'people.csv')")>]
[<InlineData("create   'new.csv'select   '*' from 'people.csv')")>]
[<InlineData("create   'new.csv'select   '*' from 'people.csv'")>]
let ``create query unhappy cases`` (query) =
    
    match (run Parser.createQuery query) with
    | Success(result, _, _)  -> Assert.True false
    | Failure(errorMsg, _, _) -> Assert.True true
    Assert.True(true)