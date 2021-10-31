## CQL - Csv Query Language

CQL is a a query language for performing operations on csv files. The language is verry simmular to ordanary SQL. The language have two query types Select and Create. I choose to not implement Update and Delete since their is no way to ensure the data integrety in a csv file, which you can in a SQL table. The parser is written in F# with [Fparsec](https://github.com/stephan-tolksdorf/fparsec) and supports recursive grammer.


### Exampel - select query
The following query performes an inner join on the files ```person.txt``` and ```adress.txt``` and then filter out the person above 18 which live in Sweden.
```java
select '*' from 'person.txt' as 'p' 
inner join 'adress.txt' as 'a' 'a.ownerid' = 'p.userid' 
where 2021 - 'p.birthyear' > 18 
where 'a.country' = "Sweden" || 'a.country' = "Germany" 
order by 'p.birthyear' asc 
top 3
```

This is a representation of the files ```person.txt``` and ```adress.txt```
```
person.txt                                        adress.txt
.........................................         ...................................................
userid; | name;     |  age;   | gender  |         | ownerid;  | country;  | city;       | zip       |
.........................................         ..................................................|
928;    | Lars;     | 1967;   | male    |         | 1394;     | Germany;  | Berlin;     | 43-242    |
1394;   | Klaus;    | 1989;   | male    |         | 928;      | Sweden;   | Stockholm;  | 97-1235   |
571;    | Juliette; | 1982;   | female  |         | 928;      | Norway;   | Oslo;       | 324-2425  |
352;    | Moa;      | 1993;   | female  |         | 571;      | France;   | Paris;      | 921-135   |
25;     | Teo;      | 2008;   | male    |         | 352;      | Sweden;   | Karlstad;   | 456-123   |
82;     | Olavi;    | 2006;   | male    |         | 25;       | Sweden;   | Gothenburg; | 10-412    |
122;    | Lena;     | 1996;   | female  |         | 82;       | Finland;  | Helsinki;   | 789-1     |
.........................................         | 122;      | Germany;  | Hamburg;    | 23-1346   |
                                                  ...................................................
```

The exampel query will get parsed into a domain model which looks like this
```f#
Select
  ([All], TableName ("person.txt", Some "p"),
   [Inner
      (TableName ("adress.txt", Some "a"),
       EqualityExpression
         (Equals, ColumnIdentifier (Some "a", "ownerid"),
          ColumnIdentifier (Some "p", "userid")))],
   [Where
      (EqualityExpression
         (GreaterThan,
          ArithmeticExpression
            (Subtract, Litteral (NumericLitteral 2021.0),
             ColumnIdentifier (Some "p", "birthyear")),
          Litteral (NumericLitteral 18.0)));
    Where
      (LogicalExpression
         (Or,
          EqualityExpression
            (Equals, ColumnIdentifier (Some "a", "country"),
             Litteral (StringLitteral "Sweden")),
          EqualityExpression
            (Equals, ColumnIdentifier (Some "a", "country"),
             Litteral (StringLitteral "Germany"))))],
   Some ((Some "p", "birthyear"), Ascending), 
   Some (Top 3))
```

The domain model will then get evaluated and result in the following table.
```sql
userid   name   birthyear   gender   ownerid  country   city        zip
   928   Lars   1967        male     928      Sweden    Stockholm   97-1235
   1394  Klaus  1989        male     1394     Germany   Berlin      43-242
   352   Moa    1993        female   352      Sweden    Karlstad    456-123
```
