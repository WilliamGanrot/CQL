## CQL - Csv Query Language

CQL is a a query language for performing operations on csv files. The language is verry simmular to ordanary SQL. The language have two query types Select and Create. I choose to not implement Update and Delete since their is no way to ensure the data integrety in a csv file, which you can in a SQL table. The parser is written in F# with [Fparsec](https://github.com/stephan-tolksdorf/fparsec) and supports recursive grammer.
