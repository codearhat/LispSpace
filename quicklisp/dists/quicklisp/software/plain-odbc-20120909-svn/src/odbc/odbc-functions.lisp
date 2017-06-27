;;;-*- Mode: Lisp; Package: ODBC -*-

;; ODBC module for MCL, LispWorks, CMUCL, ACL and CormanLisp
;; Version 0.9
;; Copyright (C) Paul Meurer 1999, 2000. All rights reserved.
;; paul.meurer@hit.uib.no
;;
;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp".
;; Bug reports and suggestions are highly welcome.



(in-package :plain-odbc)

(defmacro with-temporary-allocations (allocs &body body)
  `(let (,@allocs)
    (unwind-protect
         (progn ,@body)
      ,@(mapcar (lambda (alloc) (list 'cffi:foreign-free (first alloc)))
                allocs))))


(defmacro with-foreign-string-allocations (specs &body body)  
  (if (car specs)
      `(with-foreign-string-alloc ,(car specs)
	 (with-foreign-string-allocations ,(cdr specs) ,@body))
      `(progn ,@body)))




;;; rav:
;;; primitive error handling
;;; maybe one could create sub conditions based on the sql-state?
;;; but not even jdbc does this

(define-condition sql-condition (condition)
  ((error-message :initarg :error-message)
   (error-code :initarg :error-code)
   (sql-state :initarg :sql-state))
  (:report (lambda (condition stream) 
             (format stream "~A, error code ~A, State: ~A." 
                     (slot-value condition 'error-message)
                     (slot-value condition 'error-code)
                     (slot-value condition 'sql-state)))))

(define-condition sql-error (sql-condition error)
  ())

(define-condition sql-warning (sql-condition warning)
  ())

;; TODO: Why doesn't this use with-temporary-allocations? -dso
(defun handle-error (henv hdbc hstmt)
  (with-temporary-allocations
      ((sql-state (alloc-chars 256))
       (error-message (alloc-chars #.$SQL_MAX_MESSAGE_LENGTH))
       (error-code (cffi:foreign-alloc 'sql-integer))
       (msg-length (cffi:foreign-alloc 'sql-small-int)))
    (SQLError (or henv (cffi:null-pointer))
              (or hdbc (cffi:null-pointer))
              hstmt sql-state
              error-code error-message
              $SQL_MAX_MESSAGE_LENGTH msg-length)
    (values
     (get-string-nts error-message)
     (get-string-nts sql-state)
     (cffi:mem-ref msg-length 'sql-small-int)
     (cffi:mem-ref error-code 'sql-integer))))

(defun sql-state (henv hdbc hstmt)
  (with-temporary-allocations
      ((sql-state (cffi:foreign-alloc :char :count 256))
       (error-message (cffi:foreign-alloc :char :count $SQL_MAX_MESSAGE_LENGTH))
       (error-code (cffi:foreign-alloc 'sql-integer))
       (msg-length (cffi:foreign-alloc 'sql-small-int)))
    (SQLError 
     (or henv (cffi:null-pointer))
     (or hdbc (cffi:null-pointer))
     hstmt sql-state error-code
     error-message $SQL_MAX_MESSAGE_LENGTH msg-length)
    (get-string sql-state 5)          ;(%cstring-to-keyword sql-state)
    ))

;;; rav:
;;; instead of a big macro use a fucntion

(defun error-handling-fun (result-code henv hdbc hstmt)
  (case result-code 
    ((#.$SQL_SUCCESS #.$SQL_NO_DATA_FOUND) (values result-code nil))
    ((#.$SQL_SUCCESS_WITH_INFO #.$SQL_ERROR)
      (multiple-value-bind (error-message sql-state msg-length error-code)
          (handle-error (or henv (cffi:null-pointer))
                        (or hdbc (cffi:null-pointer))
                        (or hstmt (cffi:null-pointer)))
        (declare (ignore msg-length)) 
        (values result-code
                (make-condition
                 (if (eql result-code #.$SQL_SUCCESS_WITH_INFO)
                   'sql-warning
                   'sql-error)
                 :error-message error-message
                 :sql-state sql-state
                 :error-code error-code))))
    ; this can happen, using  a wrong handle
    (#.$SQL_INVALID_HANDLE
      (values result-code
              (make-condition 'sql-error :error-message "[ODBC error] Invalid handle"
                              :sql-state nil
                              :error-code nil
                              )))
    ;; maybe this should raise an error immediately
    (#.$SQL_STILL_EXECUTING
      (values result-code 
              (make-condition 'sql-error :error-message"[ODBC error] Still executing"
                              :sql-state nil
                              :error-code nil)))
    (#.$SQL_NEED_DATA
     (values result-code nil))

    ;; rav: hope above are all result codes I know
    (otherwise (error "unknown result of odbc execution: ~A" result-code))
    ))


;;; rav:
;;; but the remaining macro is still large

(defmacro with-error-handling ((&key henv hdbc hstmt)
                                   odbc-call &body body)
  (let ((condition-var (gensym))
        (result-code (gensym)))
    `(multiple-value-bind (,result-code ,condition-var) 
      (error-handling-fun ,odbc-call ,henv ,hdbc ,hstmt)
      (typecase ,condition-var 
        (warning (warn ,condition-var))
        (error (error ,condition-var)))
      ,result-code ,@body)))


(defun %new-environment-handle ()
  (cffi:with-foreign-object (phenv 'sql-h-env)
    (with-error-handling
        ()
        (SQLAllocEnv phenv)
      (cffi:mem-ref phenv 'sql-h-env))))

(defun %new-db-connection-handle (henv)
  (cffi:with-foreign-object (phdbc 'sql-h-dbc)
    (with-error-handling
        (:henv henv)
        (SQLAllocConnect henv phdbc)
      (cffi:mem-ref phdbc 'sql-h-dbc))))

(defun %free-statement (hstmt option)
  (with-error-handling
      (:hstmt hstmt)
      (SQLFreeStmt
       hstmt
       (ecase option
         (:drop $SQL_DROP)
         (:close $SQL_CLOSE)
         (:unbind $SQL_UNBIND)
         (:reset $SQL_RESET_PARAMS)))))

(defun %free-connection (hdbc)
  (with-error-handling
   (:hdbc hdbc)
   (SQLFreeConnect hdbc)))



(defmacro with-statement-handle ((hstmt hdbc) &body body)
  `(let ((,hstmt (%new-statement-handle ,hdbc)))
     (unwind-protect
       (progn ,@body)
       (%free-statement ,hstmt :drop))))

;; functional interface

;;
(defun %sql-driver-connect (henv hdbc connection-string completion-option)
  (declare (string connection-string))
  (let ((completion-option
         (ecase completion-option
           (:complete $SQL_DRIVER_COMPLETE)
           (:required $SQL_DRIVER_COMPLETE_REQUIRED)
           (:prompt $SQL_DRIVER_PROMPT)
           (:noprompt $SQL_DRIVER_NOPROMPT))))
    (with-foreign-string-alloc (connection-str-ptr connection-string)
      (with-temporary-allocations
          ((complete-connection-str-ptr (alloc-chars 1024))
           (length-ptr (cffi:foreign-alloc 'sql-small-int)))
        (with-error-handling
            (:henv henv :hdbc hdbc)
            
            (SQLDriverConnect hdbc
                              (cffi:null-pointer) ; no window
                              connection-str-ptr ; TODO: How does
						 ; encoding affect the
						 ; length?
                              (length connection-string) ;$SQL_NTS
                              complete-connection-str-ptr
                              1024
                              length-ptr
                              completion-option))
        (get-string-nts complete-connection-str-ptr)))))

(defun %disconnect (hdbc)
  (with-error-handling
      (:hdbc hdbc)
      (SQLDisconnect hdbc)))

(defun %commit (henv hdbc)
  (with-error-handling
      (:henv henv :hdbc hdbc)
      (SQLTransact
       henv hdbc $SQL_COMMIT)))

(defun %rollback (henv hdbc)
  (with-error-handling
      (:henv henv :hdbc hdbc)
      (SQLTransact
       henv hdbc $SQL_ROLLBACK)))

; col-nr is zero-based in Lisp
; col-nr = :bookmark retrieves a bookmark.
(defun %bind-column (hstmt column-nr c-type data-ptr precision out-len-ptr)
  (declare (type (integer 0) column-nr))
  (with-error-handling
      (:hstmt hstmt)
      (SQLBindCol hstmt
                  (if (eq column-nr :bookmark) 0 (1+ column-nr))
                  c-type data-ptr precision out-len-ptr)))

; parameter-nr is zero-based in Lisp
(defun %sql-bind-parameter (hstmt parameter-nr parameter-type c-type
                            sql-type precision scale data-ptr
                            max-value out-len-ptr)
  (declare (type (integer 0) parameter-nr))
  (with-error-handling
      (:hstmt hstmt)
      (SQLBindParameter hstmt (1+ parameter-nr)
                        parameter-type  ;$SQL_PARAM_INPUT
                        c-type          ;$SQL_C_CHAR
                        sql-type        ;$SQL_VARCHAR
                        precision       ;(1- (length str))
                        scale           ;0
                        data-ptr
                        max-value
                        out-len-ptr     ;#.(cffi:null-pointer)
                        )))

(defun %sql-fetch (hstmt)
  (with-error-handling 
      (:hstmt hstmt)
      (SQLFetch hstmt)))

(defun %new-statement-handle (hdbc)
  (with-temporary-allocations
      ((hstmt-ptr (cffi:foreign-alloc 'sql-h-stmt)))
    (with-error-handling
        (:hdbc hdbc)
        (SQLAllocStmt hdbc hstmt-ptr)
      (cffi:mem-ref hstmt-ptr 'sql-h-stmt))))

(defun %sql-get-info (hdbc info-type)
  (ecase info-type
    ;; those return string
    ((#.$SQL_ACCESSIBLE_PROCEDURES
      #.$SQL_ACCESSIBLE_TABLES
      #.$SQL_COLUMN_ALIAS
      #.$SQL_DATA_SOURCE_NAME
      #.$SQL_DATA_SOURCE_READ_ONLY
      #.$SQL_DBMS_NAME
      #.$SQL_DBMS_VER
      #.$SQL_DRIVER_NAME
      #.$SQL_DRIVER_ODBC_VER
      #.$SQL_DRIVER_VER
      #.$SQL_EXPRESSIONS_IN_ORDERBY
      #.$SQL_IDENTIFIER_QUOTE_CHAR
      #.$SQL_KEYWORDS
      #.$SQL_LIKE_ESCAPE_CLAUSE
      #.$SQL_MAX_ROW_SIZE_INCLUDES_LONG
      #.$SQL_MULT_RESULT_SETS
      #.$SQL_MULTIPLE_ACTIVE_TXN
      #.$SQL_NEED_LONG_DATA_LEN
      #.$SQL_ODBC_SQL_OPT_IEF
      #.$SQL_ODBC_VER
      #.$SQL_ORDER_BY_COLUMNS_IN_SELECT
      #.$SQL_OUTER_JOINS
      #.$SQL_OWNER_TERM
      #.$SQL_PROCEDURE_TERM
      #.$SQL_PROCEDURES
      #.$SQL_QUALIFIER_NAME_SEPARATOR
      #.$SQL_QUALIFIER_TERM
      #.$SQL_ROW_UPDATES
      #.$SQL_SEARCH_PATTERN_ESCAPE
      #.$SQL_SERVER_NAME
      #.$SQL_SPECIAL_CHARACTERS
      #.$SQL_TABLE_TERM
      #.$SQL_USER_NAME)
     (with-temporary-allocations
         ((info-ptr (alloc-chars 1024))
          (info-length-ptr (cffi:foreign-alloc 'sql-small-int)))
       (with-error-handling
           (:hdbc hdbc)
           #-pcl
         (SQLGetInfo hdbc info-type info-ptr 1023 info-length-ptr)
         #+pcl
         (SQLGetInfo-Str hdbc info-type info-ptr 1023 info-length-ptr)
         ;; TODO: I believe the following assumes that the buffer was
         ;; big enough to include the null-terminator.
         (get-string-nts info-ptr))))
    ;; those returning a 16-bit integer
    ((#.$SQL_ACTIVE_CONNECTIONS
      #.$SQL_ACTIVE_STATEMENTS
      #.$SQL_CONCAT_NULL_BEHAVIOR
      #.$SQL_CORRELATION_NAME
      #.$SQL_CURSOR_COMMIT_BEHAVIOR
      #.$SQL_CURSOR_ROLLBACK_BEHAVIOR
      #.$SQL_MAX_COLUMN_NAME_LEN
      #.$SQL_MAX_COLUMNS_IN_GROUP_BY
      #.$SQL_MAX_COLUMNS_IN_INDEX
      #.$SQL_MAX_COLUMNS_IN_ORDER_BY
      #.$SQL_MAX_COLUMNS_IN_SELECT
      #.$SQL_MAX_COLUMNS_IN_TABLE
      #.$SQL_MAX_CURSOR_NAME_LEN
      #.$SQL_MAX_OWNER_NAME_LEN
      #.$SQL_MAX_PROCEDURE_NAME_LEN
      #.$SQL_MAX_QUALIFIER_NAME_LEN
      #.$SQL_MAX_TABLE_NAME_LEN
      #.$SQL_MAX_TABLES_IN_SELECT
      #.$SQL_MAX_USER_NAME_LEN
      #.$SQL_NON_NULLABLE_COLUMNS
      #.$SQL_NULL_COLLATION
      #.$SQL_ODBC_API_CONFORMANCE
      #.$SQL_ODBC_SAG_CLI_CONFORMANCE
      #.$SQL_ODBC_SQL_CONFORMANCE
      #.$SQL_QUALIFIER_LOCATION
      #.$SQL_QUOTED_IDENTIFIER_CASE
      #.$SQL_TXN_CAPABLE)
     (with-temporary-allocations
         ((info-ptr (cffi::foreign-alloc 'sql-small-int))
          (info-length-ptr (cffi::foreign-alloc 'sql-small-int)))
       (with-error-handling
           (:hdbc hdbc)
           (SQLGetInfo hdbc
                       info-type
                       info-ptr
                       0
                       info-length-ptr)
         (cffi:mem-ref info-ptr 'sql-small-int))))
    ;; those returning a 32-bit bitmask
    ((#.$SQL_ALTER_TABLE
      #.$SQL_BOOKMARK_PERSISTENCE
      #.$SQL_CONVERT_BIGINT
      #.$SQL_CONVERT_BINARY
      #.$SQL_CONVERT_BIT
      #.$SQL_CONVERT_CHAR
      #.$SQL_CONVERT_DATE
      #.$SQL_CONVERT_DECIMAL
      #.$SQL_CONVERT_DOUBLE
      #.$SQL_CONVERT_FLOAT
      #.$SQL_CONVERT_INTEGER
      #.$SQL_CONVERT_LONGVARCHAR
      #.$SQL_CONVERT_NUMERIC
      #.$SQL_CONVERT_REAL
      #.$SQL_CONVERT_SMALLINT
      #.$SQL_CONVERT_TIME
      #.$SQL_CONVERT_TIMESTAMP
      #.$SQL_CONVERT_TINYINT
      #.$SQL_CONVERT_VARBINARY
      #.$SQL_CONVERT_VARCHAR
      #.$SQL_CONVERT_LONGVARBINARY
      #.$SQL_CONVERT_FUNCTIONS
      #.$SQL_FETCH_DIRECTION
      #.$SQL_FILE_USAGE
      #.$SQL_GETDATA_EXTENSIONS
      #.$SQL_LOCK_TYPES
      #.$SQL_MAX_INDEX_SIZE
      #.$SQL_MAX_ROW_SIZE
      #.$SQL_MAX_STATEMENT_LEN
      #.$SQL_NUMERIC_FUNCTIONS
      #.$SQL_OWNER_USAGE
      #.$SQL_POS_OPERATIONS
      #.$SQL_POSITIONED_STATEMENTS
      #.$SQL_QUALIFIER_USAGE
      #.$SQL_SCROLL_CONCURRENCY
      #.$SQL_SCROLL_OPTIONS
      #.$SQL_STATIC_SENSITIVITY
      #.$SQL_STRING_FUNCTIONS
      #.$SQL_SUBQUERIES
      #.$SQL_SYSTEM_FUNCTIONS
      #.$SQL_TIMEDATE_ADD_INTERVALS
      #.$SQL_TIMEDATE_DIFF_INTERVALS
      #.$SQL_TIMEDATE_FUNCTIONS
      #.$SQL_TXN_ISOLATION_OPTION
      #.$SQL_UNION)
     (with-temporary-allocations
         ;; TODO: It'd be nice to have this as a sql-* type.  However,
         ;; while the X/Open spec is usually quiet about data sizes,
         ;; it specifically says a 32-bit bitmask for these; so if
         ;; SQL-INTEGER changes to 64-bit, these may or may not change
         ;; as well. -dso
         ((info-ptr (cffi:foreign-alloc :uint32))
          (info-length-ptr (cffi:foreign-alloc 'sql-small-int)))
       (with-error-handling
           (:hdbc hdbc)
           (SQLGetInfo hdbc
                       info-type
                       info-ptr
                       0
                       info-length-ptr)
         (cffi:mem-ref info-ptr :uint32))))
    ;; those returning an integer
    ((#.$SQL_DEFAULT_TXN_ISOLATION
      #.$SQL_DRIVER_HDBC
      #.$SQL_DRIVER_HENV
      #.$SQL_DRIVER_HLIB
      #.$SQL_DRIVER_HSTMT
      #.$SQL_GROUP_BY
      #.$SQL_IDENTIFIER_CASE
      #.$SQL_MAX_BINARY_LITERAL_LEN
      #.$SQL_MAX_CHAR_LITERAL_LEN
      #.$SQL_ACTIVE_ENVIRONMENTS)
     (with-temporary-allocations
         ((info-ptr (cffi:foreign-alloc 'sql-integer))
          (info-length-ptr (cffi:foreign-alloc 'sql-small-int)))
       (with-error-handling
           (:hdbc hdbc)
           (SQLGetInfo hdbc info-type info-ptr 0 info-length-ptr)
         (cffi:mem-ref info-ptr 'sql-integer))))))

(defun %sql-exec-direct (sql hstmt henv hdbc)
  (declare (string sql))
  (with-foreign-string-alloc (sql-ptr sql)
    (with-error-handling
        (:hstmt hstmt :henv henv :hdbc hdbc)
        (SQLExecDirect hstmt sql-ptr $SQL_NTS))))

(defun %sql-execute (hstmt)
  (with-error-handling
      (:hstmt hstmt)
      (SQLExecute hstmt)))

(defun result-columns-count (hstmt)
  (with-temporary-allocations 
      ((columns-nr-ptr (cffi:foreign-alloc 'sql-small-int)))
    (with-error-handling (:hstmt hstmt)
        (SQLNumResultCols hstmt columns-nr-ptr)
      (cffi:mem-ref columns-nr-ptr 'sql-small-int))))

(defun result-rows-count (hstmt)
  (with-temporary-allocations 
      ((row-count-ptr (cffi:foreign-alloc 'sql-len)))
    (with-error-handling (:hstmt hstmt)
        (SQLRowCount hstmt row-count-ptr)
      (cffi:mem-ref row-count-ptr 'sql-len))))


;;; fixme, the whole column descriptiopn stuff should be put into one loop
;;; so that we can reuse the allocations

;; Column counting is 1-based
(defun %describe-column (hstmt column-nr)
  (declare (type (integer 1) column-nr))
  (with-temporary-allocations
      ((column-name-ptr (alloc-chars 256))
       (column-name-length-ptr (cffi:foreign-alloc 'sql-small-int))
       (column-sql-type-ptr (cffi:foreign-alloc 'sql-small-int))
       (column-precision-ptr (cffi:foreign-alloc 'sql-u-len))
       (column-scale-ptr (cffi:foreign-alloc 'sql-small-int))
       (column-nullable-p-ptr (cffi:foreign-alloc 'sql-small-int)))
    (with-error-handling (:hstmt hstmt)
        (SQLDescribeCol hstmt column-nr column-name-ptr 256
                        column-name-length-ptr
                        column-sql-type-ptr
                        column-precision-ptr
                        column-scale-ptr
                        column-nullable-p-ptr)
      (values
       (get-string-nts column-name-ptr)
       (cffi:mem-ref column-sql-type-ptr 'sql-small-int)
       (cffi:mem-ref column-precision-ptr 'sql-u-len)
       (cffi:mem-ref column-scale-ptr 'sql-small-int)
       (cffi:mem-ref column-nullable-p-ptr 'sql-small-int)))))


;; fixme, include it later
#+ignore
(defun %prepare-describe-columns (hstmt table-qualifier table-owner 
                                   table-name column-name)
  (with-tempo (table-qualifier-ptr table-qualifier)
    (with-cstr (table-owner-ptr table-owner) 
      (with-cstr (table-name-ptr table-name)
        (with-cstr (column-name-ptr column-name)
          (with-error-handling
            (:hstmt hstmt) 
            (SQLColumns hstmt
                        table-qualifier-ptr (length table-qualifier)
                        table-owner-ptr (length table-owner)
                        table-name-ptr (length table-name)
                        column-name-ptr (length column-name))))))))

;;; rav, currently not needed
;; resultset returning odbc functions should be
;; integrated with the query objects

#+ignore
(defun %describe-columns (hdbc table-qualifier table-owner 
                                   table-name column-name)
  (with-statement-handle (hstmt hdbc)
    (%prepare-describe-columns hstmt table-qualifier table-owner 
                               table-name column-name)
    (fetch-all-rows hstmt)))



(defun %sql-prepare (hstmt sql)
  (declare (string sql))
  (with-foreign-string-alloc (sql-ptr sql)
    (with-error-handling (:hstmt hstmt)
        (SQLPrepare hstmt sql-ptr $SQL_NTS))))

(defun %sql-primary-keys (hstmt catalog-name schema-name table-name)
  
  (with-foreign-string-allocations 
   ((catalog-name-ptr (or catalog-name ""))
    (schema-name-ptr (or schema-name ""))
    (table-name-ptr (or table-name "")))
   (with-error-handling (:hstmt hstmt)
                        (SQLPrimaryKeys hstmt
                                        (if catalog-name catalog-name-ptr (cffi:null-pointer))
                                        $SQL_NTS
                                        (if schema-name schema-name-ptr (cffi:null-pointer))
                                        $SQL_NTS
                                        (if table-name table-name-ptr (cffi:null-pointer))
                                        $SQL_NTS))))

(defun %sql-tables (hstmt catalog-name schema-name table-name table-type)
   (with-foreign-string-allocations 
    ((catalog-name-ptr (or catalog-name ""))
     (schema-name-ptr (or schema-name ""))
     (table-name-ptr (or table-name ""))
     (table-type-ptr (or table-type "")))
    (with-error-handling 
     (:hstmt hstmt)
     (SQLTables hstmt
      (if catalog-name catalog-name-ptr (cffi:null-pointer))
      $SQL_NTS
      (if schema-name schema-name-ptr (cffi:null-pointer))
      $SQL_NTS
      (if table-name table-name-ptr (cffi:null-pointer))
      $SQL_NTS
      (if table-type table-type-ptr (cffi:null-pointer))
      $SQL_NTS))))

(defun %sql-foreign-keys (hstmt catalog-name1 schema-name1 table-name1 
                                catalog-name2 schema-name2 table-name2)
  (with-foreign-string-allocations 
   ((catalog-name-ptr1 (or catalog-name1 ""))
    (schema-name-ptr1 (or schema-name1 ""))
    (table-name-ptr1 (or table-name1 ""))
    
    (catalog-name-ptr2 (or catalog-name2 ""))
    (schema-name-ptr2 (or schema-name2 ""))
    (table-name-ptr2 (or table-name2 "")))
   
   (with-error-handling (:hstmt hstmt)
                        (SQLForeignKeys hstmt
                                        (if catalog-name1 catalog-name-ptr1 (cffi:null-pointer))
                                        $SQL_NTS
                                        (if schema-name1 schema-name-ptr1 (cffi:null-pointer))
                                        $SQL_NTS
                                        (if table-name1 table-name-ptr1 (cffi:null-pointer))
                                        $SQL_NTS
                                        (if catalog-name2 catalog-name-ptr2 (cffi:null-pointer))
                                        $SQL_NTS
                                        (if schema-name2 schema-name-ptr2 (cffi:null-pointer))
                                        $SQL_NTS
                                        (if table-name2 table-name-ptr2 (cffi:null-pointer))
                                        $SQL_NTS))))


(defun %sql-columns (hstmt catalog-name schema-name table-name column-name)
   (with-foreign-string-allocations 
    ((catalog-name-ptr (or catalog-name ""))
     (schema-name-ptr (or schema-name ""))
     (table-name-ptr (or table-name ""))
     (column-name-ptr (or column-name "")))
    (with-error-handling 
     (:hstmt hstmt)
     (SQLColumns hstmt
      (if catalog-name catalog-name-ptr (cffi:null-pointer))
      $SQL_NTS
      (if schema-name schema-name-ptr (cffi:null-pointer))
      $SQL_NTS
      (if table-name table-name-ptr (cffi:null-pointer))
      $SQL_NTS
      (if column-name column-name-ptr (cffi:null-pointer))
      $SQL_NTS))))
 
(defun set-connection-option (hdbc option param)
  (with-error-handling (:hdbc hdbc)
      (SQLSetConnectOption hdbc option param)))

(defun disable-autocommit (hdbc)
  (set-connection-option hdbc $SQL_AUTOCOMMIT $SQL_AUTOCOMMIT_OFF))

(defun enable-autocommit (hdbc)
  (set-connection-option hdbc $SQL_AUTOCOMMIT $SQL_AUTOCOMMIT_ON))


;;;***
;;; rav, 11.6.2005
;;; added tracing support

(defun set-connection-attr-integer (hdbc option val)
  (with-error-handling (:hdbc hdbc)
      (SQLSetConnectAttr_long hdbc option val 0)))

(defun set-connection-attr-string (hdbc option val)
  (with-error-handling  (:hdbc hdbc)
      (with-foreign-string-alloc (ptr val)
        ;; TODO: Null-terminator with length?
        (SQLSetConnectAttr_string hdbc option ptr (length val)))))

(defun %start-connection-trace (hdbc filename)
  (set-connection-attr-string hdbc  $SQL_ATTR_TRACEFILE	filename)
  (set-connection-attr-integer hdbc $SQL_ATTR_TRACE	$SQL_OPT_TRACE_ON))

(defun %stop-connection-trace (hdbc)
  (set-connection-attr-integer hdbc $SQL_ATTR_TRACE	$SQL_OPT_TRACE_OFF))

;;;

; column-nr is zero-based
(defun %sql-get-data (hstmt column-nr c-type data-ptr precision out-len-ptr)
  (declare (type (integer 0) column-nr))
  (with-error-handling
      (:hstmt hstmt)
      (SQLGetData hstmt (1+ column-nr)
                  c-type data-ptr precision out-len-ptr)))

(defun %sql-get-data-raw (hstmt position c-type data-ptr buffer-length ind-ptr)
  (declare (type (integer 0) position))
  (SQLGetData hstmt (1+ position)
              c-type data-ptr buffer-length ind-ptr))


(defun %sql-param-data (hstmt param-ptr)
  (with-error-handling (:hstmt hstmt)
      (SQLParamData hstmt param-ptr)))


(defun %sql-put-data (hstmt data-ptr size)
  (with-error-handling
      (:hstmt hstmt )
      (SQLPutData hstmt data-ptr size)))


(defun %sql-more-results (hstmt)
  (let ((res (SQLMoreResults hstmt)))
    (case res
      ((#.$SQL_SUCCESS_WITH_INFO #.$SQL_SUCCESS) t)
      ((#.$SQL_NO_DATA_FOUND) nil)
      (otherwise (error-handling-fun res nil nil hstmt)))))

;(defconstant $sql-data-truncated (intern "01004" :keyword))
