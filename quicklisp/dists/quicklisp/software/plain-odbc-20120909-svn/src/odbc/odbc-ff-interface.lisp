;;;-*- Mode: Lisp; Package: ODBC -*-

;; ODBC module for MCL, LispWorks, ACL and CormanLisp
;; Version 0.9
;; Copyright (C) Paul Meurer 1999 - 2001. All rights reserved.
;; paul.meurer@hit.uib.no
;;
;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp".
;; Bug reports and suggestions are highly welcome.
 
(in-package :plain-odbc)

(define-foreign-library :odbc 
    (:windows (:default "odbc32"))
  (:unix (:default "libodbc")))

(load-foreign-library :odbc)

(defctype string-ptr :pointer)



;;;; dso-

(defctype sql-small-int :int16)
(defctype sql-u-small-int :uint16)
(defctype sql-integer :int32)
(defctype sql-u-integer :uint32)
(defctype sql-pointer :pointer)
(defctype sql-len sql-integer)
(defctype sql-u-len sql-u-integer)
(defctype sql-return sql-small-int)

(defctype *sql-small-int :pointer)
(defctype *sql-integer :pointer)
(defctype *sql-len :pointer)
(defctype *sql-u-len :pointer)

(defctype sql-handle :pointer)
(defctype sql-h-env sql-handle)
(defctype sql-h-dbc sql-handle)
(defctype sql-h-stmt sql-handle)
(defctype sql-h-wnd :pointer)

(defctype *sql-h-env :pointer)
(defctype *sql-h-dbc :pointer)
(defctype *sql-h-stmt :pointer)

(defmacro defsqlfun (name (&rest args))
  `(defcfun ,name sql-return ,@args))

;;;; -dso



(defsqlfun "SQLAllocEnv"
    ((penv *sql-h-env)))

(defsqlfun "SQLAllocConnect"
    ((henv sql-h-env)                   ; HENV        henv
     (*phdbc *sql-h-dbc)))              ; HDBC   FAR *phdbc

(defsqlfun "SQLDriverConnect"
    ((hdbc sql-h-dbc)                   ; HDBC        hdbc
     (hwnd sql-h-wnd)                   ; SQLHWND     hwnd
                                        ;(*szConnStrIn string-ptr)  ; UCHAR  FAR *szConnStrIn
     (*szConnStrIn string-ptr)          ; UCHAR  FAR *szConnStrIn
     (cbConnStrIn sql-small-int)        ; SWORD       cbConnStrIn
                                        ;(*szConnStrOut string-ptr) ; UCHAR  FAR *szConnStrOut
     (*szConnStrOut string-ptr)         ; UCHAR  FAR *szConnStrOut
     (cbConnStrOutMax sql-small-int)    ; SWORD       cbConnStrOutMaxw
     (*pcbConnStrOut *sql-small-int)    ; SWORD  FAR *pcbConnStrOut
     (fDriverCompletion :unsigned-short))) ; UWORD       fDriverCompletion

(defsqlfun "SQLDisconnect"
    ((hdbc sql-h-dbc)))                 ; HDBC        hdbc

(defsqlfun "SQLAllocStmt"
    ((hdbc sql-h-dbc)                   ; HDBC        hdbc
     (*phstmt *sql-h-stmt)))            ; HSTMT  FAR *phstmt



(defsqlfun "SQLGetInfo"
    ((hdbc sql-h-dbc)                   ; HDBC        hdbc
     (fInfoType sql-u-small-int)        ; UWORD       fInfoType
     (rgbInfoValue sql-pointer)         ; PTR         rgbInfoValue
     (cbInfoValueMax sql-small-int)     ; SWORD       cbInfoValueMax
     (*pcbInfoValue *sql-small-int)))   ; SWORD  FAR *pcbInfoValue


(defsqlfun ("SQLGetInfo" SQLGetInfo-Str)
    ((hdbc sql-h-dbc)                   ; HDBC        hdbc
     (fInfoType sql-u-small-int)        ; UWORD       fInfoType
     (rgbInfoValue string-ptr)          ; PTR         rgbInfoValue
     (cbInfoValueMax sql-small-int)     ; SWORD       cbInfoValueMax
     (*pcbInfoValue *sql-small-int)))   ; SWORD  FAR *pcbInfoValue


(defsqlfun "SQLPrepare"
    ((hstmt sql-h-stmt)                 ; HSTMT       hstmt
     (*szSqlStr string-ptr)             ; UCHAR  FAR *szSqlStr
     (cbSqlStr sql-integer)))           ; SDWORD      cbSqlStr


(defsqlfun "SQLColumns"
    ((hstmt sql-h-stmt)
     (CatalogName string-ptr)
     (NameLength1 sql-small-int)
     (SchemaName string-ptr)
     (NameLength2 sql-small-int)
     (TableName string-ptr)
     (NameLength3 sql-small-int)
     (ColumnName string-ptr)
     (NameLength4 sql-small-int)))


(defsqlfun "SQLPrimaryKeys"
    ((hstmt sql-h-stmt)
     (catalog-name string-ptr)
     (catalog-name-len sql-small-int)
     (schema-name string-ptr)
     (schema-name-len sql-small-int)
     (table-name string-ptr)
     (table-name-len sql-small-int)))

(defsqlfun "SQLTables"
    ((hstmt sql-h-stmt)
     (catalog-name string-ptr)
     (catalog-name-len sql-small-int)

     (schema-name string-ptr)
     (schema-name-len sql-small-int)

     (table-name string-ptr)
     (table-name-len sql-small-int)

     (table-type string-ptr)
     (table-type-len sql-small-int)))

(defsqlfun "SQLForeignKeys" 
  ((hstmt sql-h-stmt)
   (catalog-name1 string-ptr)
   (catalog-name-len1 sql-small-int)
   (schema-name1 string-ptr)
   (schema-name-len1 sql-small-int)
   (table-name1 string-ptr)
   (table-name-len1 sql-small-int)

   (catalog-name2 string-ptr)
   (catalog-name-len2 sql-small-int)
   (schema-name2 string-ptr)
   (schema-name-len2 sql-small-int)
   (table-name2 string-ptr)
   (table-name-len2 sql-small-int)))





(defsqlfun "SQLExecute"
    ((hstmt sql-h-stmt)))               ; HSTMT       hstmt


(defsqlfun "SQLExecDirect"
    ((hstmt sql-h-stmt)                 ; HSTMT       hstmt
     (*szSqlStr string-ptr)             ; UCHAR  FAR *szSqlStr
     (cbSqlStr sql-integer)))           ; SDWORD      cbSqlStr



(defsqlfun "SQLFreeStmt"
  ((hstmt sql-h-stmt)                 ; HSTMT       hstmt
   (fOption sql-u-small-int)))        ; UWORD       fOption

(defsqlfun "SQLFreeConnect"
  ((hdbc sql-h-dbc)))                 ; HDBC        hdbc


(defsqlfun "SQLError"
    ((henv sql-h-env)                   ; HENV        henv
     (hdbc sql-h-dbc)                   ; HDBC        hdbc
     (hstmt sql-h-stmt)                 ; HSTMT       hstmt
                                        ;     (*szSqlState string-ptr)   ; UCHAR  FAR *szSqlState
     (*szSqlState string-ptr)           ; UCHAR  FAR *szSqlState
     (*pfNativeError *sql-integer)      ; SDWORD FAR *pfNativeError
                                        ;     (*szErrorMsg string-ptr)   ; UCHAR  FAR *szErrorMsg
     (*szErrorMsg string-ptr)           ; UCHAR  FAR *szErrorMsg
     (cbErrorMsgMax sql-small-int)      ; SWORD       cbErrorMsgMax
     (*pcbErrorMsg *sql-small-int)))    ; SWORD  FAR *pcbErrorMsg



(defsqlfun "SQLNumResultCols"
    ((hstmt sql-h-stmt)                 ; HSTMT       hstmt
     (*pccol *sql-small-int)))          ; SWORD  FAR *pccol


(defsqlfun "SQLRowCount"
    ((hstmt sql-h-stmt)                 ; HSTMT       hstmt
     (*pcrow *sql-len)))                ; SDWORD FAR *pcrow


(defsqlfun "SQLDescribeCol"
    ((hstmt sql-h-stmt)                 ; HSTMT       hstmt
     (icol sql-u-small-int)             ; UWORD       icol
     (*szColName string-ptr)            ; UCHAR  FAR *szColName
     (cbColNameMax sql-small-int)       ; SWORD       cbColNameMax
     (*pcbColName *sql-small-int)       ; SWORD  FAR *pcbColName
     (*pfSqlType *sql-small-int)        ; SWORD  FAR *pfSqlType
     (*pcbColDef *sql-u-len)            ; UDWORD FAR *pcbColDef
     (*pibScale *sql-small-int)         ; SWORD  FAR *pibScale
     (*pfNullable *sql-small-int)))     ; SWORD  FAR *pfNullable


(defsqlfun "SQLBindCol"
    ((hstmt sql-h-stmt)                 ; HSTMT       hstmt
     (icol sql-u-small-int)             ; UWORD       icol
     (fCType sql-small-int)             ; SWORD       fCType
     (rgbValue sql-pointer)             ; PTR         rgbValue
     (cbValueMax sql-len)               ; SDWORD      cbValueMax
     (*pcbValue *sql-len)))             ; SDWORD FAR *pcbValue

  
(defsqlfun "SQLFetch"
    ((hstmt sql-h-stmt)))               ; HSTMT       hstmt


(defsqlfun "SQLTransact"
    ((henv sql-h-env)                   ; HENV        henv
     (hdbc sql-h-dbc)                   ; HDBC        hdbc
     (fType sql-u-small-int))) ; UWORD       fType ($SQL_COMMIT or $SQL_ROLLBACK)


;; ODBC 2.0
(defsqlfun "SQLBindParameter"
    ((hstmt sql-h-stmt)                 ; HSTMT       hstmt
     (ipar sql-u-small-int)             ; UWORD       ipar
     (fParamType sql-small-int)         ; SWORD       fParamType
     (fCType sql-small-int)             ; SWORD       fCType
     (fSqlType sql-small-int)           ; SWORD       fSqlType
     (cbColDef sql-u-len)               ; UDWORD      cbColDef
     (ibScale sql-small-int)            ; SWORD       ibScale
     (rgbValue sql-pointer)             ; PTR         rgbValue
     (cbValueMax sql-len)               ; SDWORD      cbValueMax
     (*pcbValue *sql-len)))             ; SDWORD FAR *pcbValue


;; level 1
(defsqlfun "SQLGetData"
    ((hstmt sql-h-stmt)                 ; HSTMT       hstmt
     (icol sql-u-small-int)             ; UWORD       icol
     (fCType sql-small-int)             ; SWORD       fCType
     (rgbValue sql-pointer)             ; PTR         rgbValue
     (cbValueMax sql-len)               ; SDWORD      cbValueMax
     (*pcbValue *sql-len)))             ; SDWORD FAR *pcbValue


(defsqlfun "SQLParamData"
    ((hstmt sql-h-stmt)                 ; HSTMT       hstmt
     (*prgbValue sql-pointer)))         ; PTR    FAR *prgbValue


(defsqlfun "SQLPutData"
    ((hstmt sql-h-stmt)                 ; HSTMT       hstmt
     (rgbValue sql-pointer)             ; PTR         rgbValue
     (cbValue sql-len)))                ; SDWORD      cbValue


(defsqlfun "SQLSetConnectOption"
    ((hdbc sql-h-dbc)                   ; HDBC        hdbc
     (fOption sql-u-small-int)          ; UWORD       fOption
     (vParam sql-u-len)))               ; UDWORD      vParam


;;; rav, 11.6.2005
;   SQLRETURN SQLSetConnectAttr(
;   SQLHDBC     ConnectionHandle,
;   SQLINTEGER     Attribute,
;   SQLPOINTER     ValuePtr,
;   SQLINTEGER     StringLength);
;  ValuePtr 
; [Input]
; Pointer to the value to be associated with Attribute. Depending on the value of 
; Attribute, ValuePtr will be a 32-bit unsigned integer value or will point to a 
; null-terminated character string. Note that if the Attribute argument is a 
; driver-specific value, the value in ValuePtr may be a signed integer. 

(defsqlfun ("SQLSetConnectAttr" SQLSetConnectAttr_long)
    ((hdbc sql-h-dbc)                   ; HDBC        hdbc
     ;; TODO: The new def of fOption doesn't seem compatible with the
     ;; original, but matches my headers.
     (fOption sql-integer)              ; UWORD       fOption
     (pvParam sql-integer)              ; UDWORD      vParam
     (stringlength sql-integer)))


(defsqlfun ("SQLSetConnectAttr" SQLSetConnectAttr_string)
    ((hdbc sql-handle)                  ; HDBC        hdbc
     (fOption sql-integer)              ; UWORD       fOption
     (pvParam string-ptr)               ; UDWORD      vParam
     (stringlength sql-integer)))


;; level 2
(defsqlfun "SQLMoreResults"
    ((hstmt sql-h-stmt)))


  ;;; foreign type definitions

(defcstruct sql-c-time ""
            (hour   sql-u-small-int)
            (minute sql-u-small-int)
            (second sql-u-small-int))

(defcstruct sql-c-date ""
            (year  sql-small-int)
            (month sql-u-small-int)
            (day   sql-u-small-int))

(defcstruct sql-c-timestamp ""
            (year     sql-small-int)
            (month    sql-u-small-int)
            (day      sql-u-small-int)
            (hour     sql-u-small-int)
            (minute   sql-u-small-int)
            (second   sql-u-small-int)
            (fraction sql-u-integer))

(defun %put-sql-c-date (adr %year %month %day)
  (setf (foreign-slot-value adr 'sql-c-date 'year) %year)
  (setf (foreign-slot-value adr 'sql-c-date 'month) %month)
  (setf (foreign-slot-value adr 'sql-c-date 'day) %day))


(defun %put-sql-c-timestamp (adr %year %month %day %hour %minute %second
                             %fraction)
  (setf (foreign-slot-value adr 'sql-c-timestamp 'second) %second)
  (setf (foreign-slot-value adr 'sql-c-timestamp  'minute) %minute)
  (setf (foreign-slot-value adr 'sql-c-timestamp 'hour) %hour)
  (setf (foreign-slot-value adr 'sql-c-timestamp 'day) %day)
  (setf (foreign-slot-value adr 'sql-c-timestamp  'month) %month)
  (setf (foreign-slot-value adr 'sql-c-timestamp 'year) %year)
  (setf (foreign-slot-value adr 'sql-c-timestamp 'fraction) %fraction))

(defun timestamp-to-universal-time (adr)
  (with-foreign-slots
      ((year month day hour minute second fraction) adr sql-c-timestamp)
    (values
     (encode-universal-time
      second
      minute 
      hour 
      day 
      month 
      year)
     fraction)))


(defun date-to-universal-time (adr)
  (with-foreign-slots
      ((year month day) adr sql-c-date)
    (encode-universal-time
     0 0 0
     day
     month
     year)))

(defun %sql-len-data-at-exec (length)
  (- $SQL_LEN_DATA_AT_EXEC_OFFSET length))
