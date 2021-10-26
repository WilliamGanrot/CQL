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
[<InlineData("select '*' from 'people.csv' inner join 'table' 'a' = 'v'")>]
[<InlineData("select '*' from ( select '*' from (select 'name' from 'table') )")>]
[<InlineData("select '*' from ( select '*' from (select 'name' from 'table' left join 'x' 'wef' = 24) )")>]
[<InlineData("select '*' from 'people.csv' as 'p'")>]
[<InlineData("select '*' from 'people.csv' inner join 'table' as 'p' 'a' = 'v'")>]
[<InlineData("select '*' from 'people.csv' as 'vert' inner join 'table' as 'p' 'a' = 'v'")>]
[<InlineData("select '*' from 'people.csv' as  'ta'    inner join 'table' as    'p'     'a' = 'v'")>]
[<InlineData("select '*', 'a.v' from 'people.csv' as  'ta'    inner join 'table' as    'p'     'a' = 'v' ")>]
[<InlineData("select '*' from ( select 'wef', 'qqko', 'wef.qq' from (select 'name' from 'table') )")>]
[<InlineData("select '*' from 'people.csv' as 'ta' inner join 'table' as 'p' 'a.w'  =  'v'")>]
let ``select query happy cases`` (query) =

    match (run Parser.selectQueryWitheof query) with
    | Success(result, _, _)  -> Assert.True true
    | Failure(errorMsg, _, _) -> Assert.True false

[<Theory>]
[<InlineData("select , 'adress' from 'people.csv' where ('id' = 3) where ('name' = \"bert\")")>]
[<InlineData("select name, 'adress' from 'people.csv' where ('id' = 3)")>]
[<InlineData("select from 'people.csv'")>]
[<InlineData("selectfrom 'people.csv' where ('id' = 3) where ('name' = \"bert\")")>]
[<InlineData("selectfrom 'people.csv' where ('id' = 3)")>]
[<InlineData("select '*' from people.csv where ('id' = 3)")>]
[<InlineData("select '*' from 'people.csv' whered '(id' = 2)")>]
[<InlineData("select '*' from whered ('id' = 2)")>]
[<InlineData("select '*' from (select '*' from 'name.csv'")>]
[<InlineData("select '*' from select '*' from 'name.csv'")>]
[<InlineData("select '*' from select '*' from ")>]
[<InlineData("select '*' from 'people.csv' iner join 'table' 'a' = 'v'")>]
[<InlineData("select '*' from 'people.csv' iner join 'table' 'a' = 'v' where ('x' <> 'z')")>]
[<InlineData("select '* ' 'a.v' from 'people.csv' as  'ta'    inner join 'table' as    'p'     'a' = 'v'")>]
[<InlineData("select '*' 'a .v' from 'people.csv' as  'ta'    inner join 'table' as    'p'     'a' = 'v'")>]
[<InlineData("select '*' 'a. v' from 'people.csv' as  'ta'    inner join 'table' as    'p'     'a' = 'v'")>]
[<InlineData("select '*' 'a-v' from 'people.csv' as  'ta'    inner join 'table' as    'p'     'a' = 'v'")>]
[<InlineData("select '* from ( select 'wef', 'qqko', 'wef.qq' from (select 'name' from 'table') )")>]
let ``select query unhappy cases`` (query) =

    match (run Parser.selectQueryWitheof query) with
    | Success(result, _, _)  -> Assert.True false
    | Failure(errorMsg, _, _) -> Assert.True true

[<Theory>]
[<InlineData("create 'new.csv' (select 'name', 'adress' from 'people.csv' where 'id' = 3 where 'name' = \"bert\"  )  ")>]
[<InlineData("create 'new.csv' (  select '*' from 'people.csv')")>]
[<InlineData("  create   'new.csv'(select   '*' from 'people.csv')")>]
[<InlineData("  create   'new.csv'(select   '*' from 'people.csv' as 'v') as 'kart'")>]
let ``create query happy cases`` (query) =
    
    match (run Parser.createQuery query) with
    | Success(result, _, _)  -> Assert.True true
    | Failure(errorMsg, _, _) -> Assert.True false


[<Theory>]
[<InlineData("create  (select 'name', 'adress' from 'people.csv' where 'id' = 3 where 'name' = \"bert\")")>]
[<InlineData("create'new.csv' (select '*' from 'people.csv')")>]
[<InlineData("create   'new.csv'select   '*' from 'people.csv')")>]
[<InlineData("create   'new.csv'select   '*' from 'people.csv'")>]
let ``create query unhappy cases`` (query) =
    
    match (run Parser.createQuery query) with
    | Success(result, _, _)  -> Assert.True false
    | Failure(errorMsg, _, _) -> Assert.True true

[<Theory>]
[<InlineData("inner join 'table' 'a' = 'v'")>]
let ``joinType happy cases`` (query) =
    
    match (run Parser.joinType query) with
    | Success(result, _, _)  -> Assert.True true
    | Failure(errorMsg, _, _) -> Assert.True false