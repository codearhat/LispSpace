;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;; plain-odbc, ODBC module for clisp
;; Copyright (C) Roland Averkamp 2005
;; Roland.Averkamp@gmx.de
;; the license agreement can be found in file license.txt

(in-package :common-lisp-user) 

(defpackage "PLAIN-ODBC"
  (:use 
   "COMMON-LISP"  "CFFI")
;  rav, 20.12.2007 I do not think this is necessary anymore
; #+mcl "CCL" #+cormanlisp "WIN32" "CFFI")
  (:export
   "EXEC-SQL"
   "EXEC-QUERY" 
   "EXEC-UPDATE" 
   "EXEC-COMMAND"

   "EXEC-SQL*"
   "EXEC-QUERY*" 
   "EXEC-UPDATE*" 
   "EXEC-COMMAND*"

   "PREPARE-STATEMENT" 
   "EXEC-PREPARED-QUERY" 
   "EXEC-PREPARED-UPDATE"
   "EXEC-PREPARED-COMMAND"
   "FREE-STATEMENT" 
   "CONNECT"   

   ;Metadata
   "GET-PRIMARY-KEYS"
   "GET-FOREIGN-KEYS"
   "GET-TABLES"
   "GET-COLUMNS"
   ;"DRIVER-CONNECT"
   "CONNECT-GENERIC"
   "CLOSE-CONNECTION"
   "COMMIT"
   "ROLLBACK"
   
   "WITH-PREPARED-STATEMENT"
   
   "*UNIVERSAL-TIME-TO-DATE-DATAYPE*"
   "*DATE-DATATYPE-TO-UNIVERSAL-TIME*"
   "*DATE-TYPE-PREDICATE*"

   ;; utilities
   "*DEFAULT-ACCESS-DSN*"
   "*DEFAULT-ORACLE-DSN*"
   "*DEFAULT-SQL-SERVER-DSN*"
   "*DEFAULT-MYSQL-DSN*"
   "*DEFAULT-SQLITE-DSN*"

   
   "USE-BIND-COLUMN"

   "CONNECT-ACCESS"
   "CONNECT-SQL-SERVER"
   "CONNECT-ORACLE" 
   "CONNECT-MYSQL"
   "CONNECT-SQLITE" 

   "TRACE-CONNECTION"
   "UNTRACE-CONNECTION"


)) 
 
