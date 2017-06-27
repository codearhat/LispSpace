 
              Testing

For every kind of database there is a file, named according to
the database.
Currently these are:

test-oracle.lisp  
test-sql-server.lisp 
test-mysql.lisp

A file contains tests for the database.

All these files are loaded by the file test-main.lisp.
This file contains also some utility procedures needed by the other files.


The tests are started with the function run-mysql-tests,run-oracle-tests
and run-sql-server-tests. These function take one parameter, an
odbc connection to the corresponding database.

[44]> (setf *con* (connect-oracle "ltrav1" "scott" "tiger"))

#<ODBC-CONNECTION SERVER="ltrav1" DBMS="Oracle" USER="scott">
[45]>  (test-plain-odbc:run-oracle-tests *con*)

NIL



