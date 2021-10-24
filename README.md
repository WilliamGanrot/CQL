## CQL - Csv Query Language

CQL is a a query language for performing operations on csv files. The language is verry simmular to ordanary SQL. The language have two query types Select and Create. I choose to not implement Update and Delete since their is no way to ensure the data integrety in a csv file, which you can in a SQL table. The parser is written in F# with [Fparsec](https://github.com/stephan-tolksdorf/fparsec) and supports recursive grammer.


### Exampel - joining files
The following query performes an inner join on the files ```person.txt``` and ```adress.txt``` and fetches all the rows.
```sql
select '*' from 'person.txt' as 'p' 
inner join 'adress.txt' as 'a' 'a.ownerid' = 'p.userid'
```

This is a representation of the files ```person.txt``` and ```adress.txt```
```
PERSON.TXT                                          ADRESS.TXT
...........................................         .......................................................
userid; | name;      |  age;  | gender    |         | ownerid;  | country;    | city;       | zip         |
...........................................         .......................................................
928;    |"Lars";     |  54;   | "male"    |         | 1394;     | "Germany";  | "Berlin";   | "43-242"    |
1394;   |"Klaus";    |  32;   | "male"    |         | 928;      | "Sweden";"  | "Stockholm";| "97-1235"   |
571;    |"Juliette"; |  39;   | "female"  |         | 928;      | "Norway";"  | "Oslo";     | "324-2425"  |
...........................................         | 571;      | "France";"  | "Paris";    | "921-135"   |
                                                    .......................................................
```

The exampel query will get parsed into a domain model which looks like this
```f#
Select
  ([All], TableName ("person.txt", Some "p"),
   [Inner
      (TableName ("adress.txt", Some "a"),
       [Binary
          (Equals, ColumnExpression (Some "a", "ownerid"),
           ColumnExpression (Some "p", "userid"))])], [])
```

The domain model will then get evaluated and result in the following table.
```sql
userid        name       age    gender   ownerid   country      city         zip
   928      "Lars"        54    "male"       928  "Sweden""Stockholm"  "97-1235"
   928      "Lars"        54    "male"       928  "Norway"    "Oslo"  "324-2425"
  1394     "Klaus"        32    "male"      1394 "Germany"  "Berlin"    "43-242"
   571  "Juliette"        39  "female"       571  "France"   "Paris"   "921-135"
```
