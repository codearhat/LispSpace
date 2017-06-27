;;; WARNING: This is a generated file, editing it is not advised!
(in-package :common-lisp-user)
(defpackage :hu.dwim.rdbms.sqlite.cffi (:use :cffi)
            (:export "+SQLITE-INDEX-CONSTRAINT-MATCH+" "+SQLITE-INDEX-CONSTRAINT-GE+"
             "+SQLITE-INDEX-CONSTRAINT-LT+" "+SQLITE-INDEX-CONSTRAINT-LE+"
             "+SQLITE-INDEX-CONSTRAINT-GT+" "+SQLITE-INDEX-CONSTRAINT-EQ+"
             "+SQLITE-UTF-16-ALIGNED+" "+SQLITE-ANY+" "+SQLITE-UTF-16+" "+SQLITE-UTF-16-BE+"
             "+SQLITE-UTF-16-LE+" "+SQLITE-UTF-8+" "+SQLITE-3-TEXT+" "+SQLITE-TEXT+"
             "+SQLITE-NULL+" "+SQLITE-BLOB+" "+SQLITE-FLOAT+" "+SQLITE-INTEGER+" "+SQLITE-COPY+"
             "+SQLITE-FUNCTION+" "+SQLITE-DROP-VTABLE+" "+SQLITE-CREATE-VTABLE+" "+SQLITE-ANALYZE+"
             "+SQLITE-REINDEX+" "+SQLITE-ALTER-TABLE+" "+SQLITE-DETACH+" "+SQLITE-ATTACH+"
             "+SQLITE-UPDATE+" "+SQLITE-TRANSACTION+" "+SQLITE-SELECT+" "+SQLITE-READ+"
             "+SQLITE-PRAGMA+" "+SQLITE-INSERT+" "+SQLITE-DROP-VIEW+" "+SQLITE-DROP-TRIGGER+"
             "+SQLITE-DROP-TEMP-VIEW+" "+SQLITE-DROP-TEMP-TRIGGER+" "+SQLITE-DROP-TEMP-TABLE+"
             "+SQLITE-DROP-TEMP-INDEX+" "+SQLITE-DROP-TABLE+" "+SQLITE-DROP-INDEX+"
             "+SQLITE-DELETE+" "+SQLITE-CREATE-VIEW+" "+SQLITE-CREATE-TRIGGER+"
             "+SQLITE-CREATE-TEMP-VIEW+" "+SQLITE-CREATE-TEMP-TRIGGER+"
             "+SQLITE-CREATE-TEMP-TABLE+" "+SQLITE-CREATE-TEMP-INDEX+" "+SQLITE-CREATE-TABLE+"
             "+SQLITE-CREATE-INDEX+" "+SQLITE-IGNORE+" "+SQLITE-DENY+" "+SQLITE-DONE+"
             "+SQLITE-ROW+" "+SQLITE-NOTADB+" "+SQLITE-RANGE+" "+SQLITE-FORMAT+" "+SQLITE-AUTH+"
             "+SQLITE-NOLFS+" "+SQLITE-MISUSE+" "+SQLITE-MISMATCH+" "+SQLITE-CONSTRAINT+"
             "+SQLITE-TOOBIG+" "+SQLITE-SCHEMA+" "+SQLITE-EMPTY+" "+SQLITE-PROTOCOL+"
             "+SQLITE-CANTOPEN+" "+SQLITE-FULL+" "+SQLITE-NOTFOUND+" "+SQLITE-CORRUPT+"
             "+SQLITE-IOERR+" "+SQLITE-INTERRUPT+" "+SQLITE-READONLY+" "+SQLITE-NOMEM+"
             "+SQLITE-LOCKED+" "+SQLITE-BUSY+" "+SQLITE-ABORT+" "+SQLITE-PERM+" "+SQLITE-INTERNAL+"
             "+SQLITE-ERROR+" "+SQLITE-OK+" "+SQLITE-VERSION-NUMBER+" "SQLITE-3-LIBVERSION"
             "SQLITE-3-LIBVERSION-NUMBER" "SQLITE-3-CLOSE" "SQLITE-3-EXEC"
             "SQLITE-3-EXTENDED-RESULT-CODES" "SQLITE-3-LAST-INSERT-ROWID" "SQLITE-3-CHANGES"
             "SQLITE-3-TOTAL-CHANGES" "SQLITE-3-INTERRUPT" "SQLITE-3-COMPLETE"
             "SQLITE-3-COMPLETE-16" "SQLITE-3-BUSY-HANDLER" "SQLITE-3-BUSY-TIMEOUT"
             "SQLITE-3-GET-TABLE" "SQLITE-3-FREE-TABLE" "SQLITE-3-MPRINTF" "SQLITE-3-VMPRINTF"
             "SQLITE-3-SNPRINTF" "SQLITE-3-MALLOC" "SQLITE-3-REALLOC" "SQLITE-3-FREE"
             "SQLITE-3-SET-AUTHORIZER" "SQLITE-3-TRACE" "SQLITE-3-PROFILE"
             "SQLITE-3-PROGRESS-HANDLER" "SQLITE-3-OPEN" "SQLITE-3-OPEN-16" "SQLITE-3-ERRCODE"
             "SQLITE-3-ERRMSG" "SQLITE-3-ERRMSG-16" "SQLITE-3-PREPARE" "SQLITE-3-PREPARE-V-2"
             "SQLITE-3-PREPARE-16" "SQLITE-3-PREPARE-16-V-2" "SQLITE-3-BIND-BLOB"
             "SQLITE-3-BIND-DOUBLE" "SQLITE-3-BIND-INT" "SQLITE-3-BIND-INT-64" "SQLITE-3-BIND-NULL"
             "SQLITE-3-BIND-TEXT" "SQLITE-3-BIND-TEXT-16" "SQLITE-3-BIND-VALUE"
             "SQLITE-3-BIND-ZEROBLOB" "SQLITE-3-BIND-PARAMETER-COUNT"
             "SQLITE-3-BIND-PARAMETER-NAME" "SQLITE-3-BIND-PARAMETER-INDEX"
             "SQLITE-3-CLEAR-BINDINGS" "SQLITE-3-COLUMN-COUNT" "SQLITE-3-COLUMN-NAME"
             "SQLITE-3-COLUMN-NAME-16" "SQLITE-3-COLUMN-DATABASE-NAME"
             "SQLITE-3-COLUMN-DATABASE-NAME-16" "SQLITE-3-COLUMN-TABLE-NAME"
             "SQLITE-3-COLUMN-TABLE-NAME-16" "SQLITE-3-COLUMN-ORIGIN-NAME"
             "SQLITE-3-COLUMN-ORIGIN-NAME-16" "SQLITE-3-COLUMN-DECLTYPE"
             "SQLITE-3-COLUMN-DECLTYPE-16" "SQLITE-3-STEP" "SQLITE-3-DATA-COUNT"
             "SQLITE-3-COLUMN-BLOB" "SQLITE-3-COLUMN-BYTES" "SQLITE-3-COLUMN-BYTES-16"
             "SQLITE-3-COLUMN-DOUBLE" "SQLITE-3-COLUMN-INT" "SQLITE-3-COLUMN-INT-64"
             "SQLITE-3-COLUMN-TEXT" "SQLITE-3-COLUMN-TEXT-16" "SQLITE-3-COLUMN-TYPE"
             "SQLITE-3-COLUMN-VALUE" "SQLITE-3-FINALIZE" "SQLITE-3-RESET"
             "SQLITE-3-CREATE-FUNCTION" "SQLITE-3-CREATE-FUNCTION-16" "SQLITE-3-AGGREGATE-COUNT"
             "SQLITE-3-EXPIRED" "SQLITE-3-TRANSFER-BINDINGS" "SQLITE-3-GLOBAL-RECOVER"
             "SQLITE-3-VALUE-BLOB" "SQLITE-3-VALUE-BYTES" "SQLITE-3-VALUE-BYTES-16"
             "SQLITE-3-VALUE-DOUBLE" "SQLITE-3-VALUE-INT" "SQLITE-3-VALUE-INT-64"
             "SQLITE-3-VALUE-TEXT" "SQLITE-3-VALUE-TEXT-16" "SQLITE-3-VALUE-TEXT-16-LE"
             "SQLITE-3-VALUE-TEXT-16-BE" "SQLITE-3-VALUE-TYPE" "SQLITE-3-VALUE-NUMERIC-TYPE"
             "SQLITE-3-AGGREGATE-CONTEXT" "SQLITE-3-USER-DATA" "SQLITE-3-GET-AUXDATA"
             "SQLITE-3-SET-AUXDATA" "SQLITE-3-RESULT-BLOB" "SQLITE-3-RESULT-DOUBLE"
             "SQLITE-3-RESULT-ERROR" "SQLITE-3-RESULT-ERROR-16" "SQLITE-3-RESULT-ERROR-TOOBIG"
             "SQLITE-3-RESULT-INT" "SQLITE-3-RESULT-INT-64" "SQLITE-3-RESULT-NULL"
             "SQLITE-3-RESULT-TEXT" "SQLITE-3-RESULT-TEXT-16" "SQLITE-3-RESULT-TEXT-16-LE"
             "SQLITE-3-RESULT-TEXT-16-BE" "SQLITE-3-RESULT-VALUE" "SQLITE-3-RESULT-ZEROBLOB"
             "SQLITE-3-CREATE-COLLATION" "SQLITE-3-CREATE-COLLATION-V-2"
             "SQLITE-3-CREATE-COLLATION-16" "SQLITE-3-COLLATION-NEEDED"
             "SQLITE-3-COLLATION-NEEDED-16" "SQLITE-3-KEY" "SQLITE-3-REKEY" "SQLITE-3-SLEEP"
             "SQLITE-3-GET-AUTOCOMMIT" "SQLITE-3-DB-HANDLE" "SQLITE-3-COMMIT-HOOK"
             "SQLITE-3-ROLLBACK-HOOK" "SQLITE-3-UPDATE-HOOK" "SQLITE-3-ENABLE-SHARED-CACHE"
             "SQLITE-3-RELEASE-MEMORY" "SQLITE-3-SOFT-HEAP-LIMIT" "SQLITE-3-THREAD-CLEANUP"
             "SQLITE-3-TABLE-COLUMN-METADATA" "SQLITE-3-LOAD-EXTENSION"
             "SQLITE-3-ENABLE-LOAD-EXTENSION" "SQLITE-3-AUTO-EXTENSION"
             "SQLITE-3-RESET-AUTO-EXTENSION" "SQLITE-3-CREATE-MODULE" "SQLITE-3-CREATE-MODULE-V-2"
             "SQLITE-3-DECLARE-VTAB" "SQLITE-3-OVERLOAD-FUNCTION" "SQLITE-3-BLOB-OPEN"
             "SQLITE-3-BLOB-CLOSE" "SQLITE-3-BLOB-BYTES" "SQLITE-3-BLOB-READ" "SQLITE-3-BLOB-WRITE"
             "SQLITE-3-STMT" "SQLITE-3-BLOB" "SQLITE-3-VTAB" "SQLITE-3-MODULE" "SQLITE-3-CONTEXT"
             "MEM" "SQLITE-3-VTAB-CURSOR" "SQLITE-3-INDEX-INFO"
             "SQLITE-3-INDEX-INFO-SQLITE-3-INDEX-CONSTRAINT-USAGE"
             "SQLITE-3-INDEX-INFO-SQLITE-3-INDEX-ORDERBY"
             "SQLITE-3-INDEX-INFO-SQLITE-3-INDEX-CONSTRAINT" "SQLITE-3" "SQLITE-UINT-64"
             "SQLITE-3-CALLBACK" "SQLITE-3-DESTRUCTOR-TYPE" "SQLITE-3-STMT" "SQLITE-3-BLOB"
             "SQLITE-INT-64" "SQLITE-3-CONTEXT" "SQLITE-3-VALUE" "SQLITE-3-VTAB-CURSOR"
             "SQLITE-3-INDEX-INFO" "SQLITE-3-VTAB" "SQLITE-3" "SQLITE-3-MODULE"))

(in-package :hu.dwim.rdbms.sqlite.cffi)
(cffi::defctype* sqlite-3-module sqlite-3-module)
(cffi::defctype* sqlite-3 sqlite-3)
(cffi:defcstruct sqlite-3)
(cffi::defctype* sqlite-3-vtab sqlite-3-vtab)
(cffi::defctype* sqlite-3-index-info sqlite-3-index-info)
(cffi:defcstruct sqlite-3-index-info-sqlite-3-index-constraint (ic-olumn :int) (op :unsigned-char)
 (usable :unsigned-char) (it-erm-offset :int))
(cffi:defcstruct sqlite-3-index-info-sqlite-3-index-orderby (ic-olumn :int) (desc :unsigned-char))
(cffi:defcstruct sqlite-3-index-info-sqlite-3-index-constraint-usage (argv-index :int)
 (omit :unsigned-char))
(cffi:defcstruct sqlite-3-index-info (nc-onstraint :int) (ac-onstraint :pointer) (no-rder-by :int)
 (ao-rder-by :pointer) (ac-onstraint-usage :pointer) (idx-num :int) (idx-str :pointer)
 (need-to-free-idx-str :int) (order-by-consumed :int) (estimated-cost :double))
(cffi::defctype* sqlite-3-vtab-cursor sqlite-3-vtab-cursor)
(cffi:defcstruct sqlite-3-vtab-cursor (pv-tab :pointer))
(cffi::defctype* sqlite-3-value mem)
(cffi:defcstruct mem)
(cffi::defctype* sqlite-3-context sqlite-3-context)
(cffi:defcstruct sqlite-3-context)
(cffi::defctype* sqlite-int-64 :long-long)
(cffi:defcstruct sqlite-3-module (iv-ersion :int) (xc-reate :pointer) (xc-onnect :pointer)
 (xb-est-index :pointer) (xd-isconnect :pointer) (xd-estroy :pointer) (xo-pen :pointer)
 (xc-lose :pointer) (xf-ilter :pointer) (xn-ext :pointer) (xe-of :pointer) (xc-olumn :pointer)
 (xr-owid :pointer) (xu-pdate :pointer) (xb-egin :pointer) (xs-ync :pointer) (xc-ommit :pointer)
 (xr-ollback :pointer) (xf-ind-function :pointer) (xr-ename :pointer))
(cffi:defcstruct sqlite-3-vtab (pm-odule :pointer) (nr-ef :int) (ze-rr-msg :pointer))
(cffi::defctype* sqlite-3-blob sqlite-3-blob)
(cffi:defcstruct sqlite-3-blob)
(cffi::defctype* sqlite-3-stmt sqlite-3-stmt)
(cffi:defcstruct sqlite-3-stmt)
(cffi::defctype* sqlite-3-destructor-type :pointer)
(cffi::defctype* sqlite-3-callback :pointer)
(cffi::defctype* sqlite-uint-64 :unsigned-long-long)
(cl:progn
 (cffi:defcfun ("sqlite3_blob_write" sqlite-3-blob-write) :int (arg1 :pointer) (z :pointer)
  (n :int) (iOffset :int))
 (cffi:defcfun ("sqlite3_blob_read" sqlite-3-blob-read) :int (arg1 :pointer) (z :pointer) (n :int)
  (iOffset :int))
 (cffi:defcfun ("sqlite3_blob_bytes" sqlite-3-blob-bytes) :int (arg1 :pointer))
 (cffi:defcfun ("sqlite3_blob_close" sqlite-3-blob-close) :int (arg1 :pointer))
 (cffi:defcfun ("sqlite3_blob_open" sqlite-3-blob-open) :int (arg1 :pointer) (zDb :string)
  (zTable :string) (zColumn :string) (iRow sqlite-int-64) (flags :int) (ppBlob :pointer))
 (cffi:defcfun ("sqlite3_overload_function" sqlite-3-overload-function) :int (arg1 :pointer)
  (zFuncName :string) (nArg :int))
 (cffi:defcfun ("sqlite3_declare_vtab" sqlite-3-declare-vtab) :int (arg1 :pointer)
  (zCreateTable :string))
 (cffi:defcfun ("sqlite3_create_module_v2" sqlite-3-create-module-v-2) :int (db :pointer)
  (zName :string) (arg3 :pointer) (arg4 :pointer) (xDestroy :pointer))
 (cffi:defcfun ("sqlite3_create_module" sqlite-3-create-module) :int (db :pointer) (zName :string)
  (arg3 :pointer) (arg4 :pointer))
 (cffi:defcfun ("sqlite3_reset_auto_extension" sqlite-3-reset-auto-extension) :void)
 (cffi:defcfun ("sqlite3_auto_extension" sqlite-3-auto-extension) :int (xEntryPoint :pointer))
 (cffi:defcfun ("sqlite3_enable_load_extension" sqlite-3-enable-load-extension) :int (db :pointer)
  (onoff :int))
 (cffi:defcfun ("sqlite3_load_extension" sqlite-3-load-extension) :int (db :pointer)
  (zFile :string) (zProc :string) (pzErrMsg :pointer))
 (cffi:defcfun ("sqlite3_table_column_metadata" sqlite-3-table-column-metadata) :int (db :pointer)
  (zDbName :string) (zTableName :string) (zColumnName :string) (pzDataType :pointer)
  (pzCollSeq :pointer) (pNotNull :pointer) (pPrimaryKey :pointer) (pAutoinc :pointer))
 (cffi:defcfun ("sqlite3_thread_cleanup" sqlite-3-thread-cleanup) :void)
 (cffi:defcfun ("sqlite3_soft_heap_limit" sqlite-3-soft-heap-limit) :void (arg1 :int))
 (cffi:defcfun ("sqlite3_release_memory" sqlite-3-release-memory) :int (arg1 :int))
 (cffi:defcfun ("sqlite3_enable_shared_cache" sqlite-3-enable-shared-cache) :int (arg1 :int))
 (cffi:defcfun ("sqlite3_update_hook" sqlite-3-update-hook) :pointer (arg1 :pointer)
  (arg2 :pointer) (arg3 :pointer))
 (cffi:defcfun ("sqlite3_rollback_hook" sqlite-3-rollback-hook) :pointer (arg1 :pointer)
  (arg2 :pointer) (arg3 :pointer))
 (cffi:defcfun ("sqlite3_commit_hook" sqlite-3-commit-hook) :pointer (arg1 :pointer)
  (arg2 :pointer) (arg3 :pointer))
 (cffi:defcfun ("sqlite3_db_handle" sqlite-3-db-handle) :pointer (arg1 :pointer))
 (cffi:defcfun ("sqlite3_get_autocommit" sqlite-3-get-autocommit) :int (arg1 :pointer))
 (cffi:defcfun ("sqlite3_sleep" sqlite-3-sleep) :int (arg1 :int))
 (cffi:defcfun ("sqlite3_rekey" sqlite-3-rekey) :int (db :pointer) (pKey :pointer) (nKey :int))
 (cffi:defcfun ("sqlite3_key" sqlite-3-key) :int (db :pointer) (pKey :pointer) (nKey :int))
 (cffi:defcfun ("sqlite3_collation_needed16" sqlite-3-collation-needed-16) :int (arg1 :pointer)
  (arg2 :pointer) (arg3 :pointer))
 (cffi:defcfun ("sqlite3_collation_needed" sqlite-3-collation-needed) :int (arg1 :pointer)
  (arg2 :pointer) (arg3 :pointer))
 (cffi:defcfun ("sqlite3_create_collation16" sqlite-3-create-collation-16) :int (arg1 :pointer)
  (zName :string) (eTextRep :int) (arg4 :pointer) (xCompare :pointer))
 (cffi:defcfun ("sqlite3_create_collation_v2" sqlite-3-create-collation-v-2) :int (arg1 :pointer)
  (zName :string) (eTextRep :int) (arg4 :pointer) (xCompare :pointer) (xDestroy :pointer))
 (cffi:defcfun ("sqlite3_create_collation" sqlite-3-create-collation) :int (arg1 :pointer)
  (zName :string) (eTextRep :int) (arg4 :pointer) (xCompare :pointer))
 (cffi:defcfun ("sqlite3_result_zeroblob" sqlite-3-result-zeroblob) :void (arg1 :pointer) (n :int))
 (cffi:defcfun ("sqlite3_result_value" sqlite-3-result-value) :void (arg1 :pointer)
  (arg2 :pointer))
 (cffi:defcfun ("sqlite3_result_text16be" sqlite-3-result-text-16-be) :void (arg1 :pointer)
  (arg2 :pointer) (arg3 :int) (arg4 :pointer))
 (cffi:defcfun ("sqlite3_result_text16le" sqlite-3-result-text-16-le) :void (arg1 :pointer)
  (arg2 :pointer) (arg3 :int) (arg4 :pointer))
 (cffi:defcfun ("sqlite3_result_text16" sqlite-3-result-text-16) :void (arg1 :pointer)
  (arg2 :pointer) (arg3 :int) (arg4 :pointer))
 (cffi:defcfun ("sqlite3_result_text" sqlite-3-result-text) :void (arg1 :pointer) (arg2 :string)
  (arg3 :int) (arg4 :pointer))
 (cffi:defcfun ("sqlite3_result_null" sqlite-3-result-null) :void (arg1 :pointer))
 (cffi:defcfun ("sqlite3_result_int64" sqlite-3-result-int-64) :void (arg1 :pointer)
  (arg2 sqlite-int-64))
 (cffi:defcfun ("sqlite3_result_int" sqlite-3-result-int) :void (arg1 :pointer) (arg2 :int))
 (cffi:defcfun ("sqlite3_result_error_toobig" sqlite-3-result-error-toobig) :void (arg1 :pointer))
 (cffi:defcfun ("sqlite3_result_error16" sqlite-3-result-error-16) :void (arg1 :pointer)
  (arg2 :pointer) (arg3 :int))
 (cffi:defcfun ("sqlite3_result_error" sqlite-3-result-error) :void (arg1 :pointer) (arg2 :string)
  (arg3 :int))
 (cffi:defcfun ("sqlite3_result_double" sqlite-3-result-double) :void (arg1 :pointer)
  (arg2 :double))
 (cffi:defcfun ("sqlite3_result_blob" sqlite-3-result-blob) :void (arg1 :pointer) (arg2 :pointer)
  (arg3 :int) (arg4 :pointer))
 (cffi:defcfun ("sqlite3_set_auxdata" sqlite-3-set-auxdata) :void (arg1 :pointer) (arg2 :int)
  (arg3 :pointer) (arg4 :pointer))
 (cffi:defcfun ("sqlite3_get_auxdata" sqlite-3-get-auxdata) :pointer (arg1 :pointer) (arg2 :int))
 (cffi:defcfun ("sqlite3_user_data" sqlite-3-user-data) :pointer (arg1 :pointer))
 (cffi:defcfun ("sqlite3_aggregate_context" sqlite-3-aggregate-context) :pointer (arg1 :pointer)
  (nBytes :int))
 (cffi:defcfun ("sqlite3_value_numeric_type" sqlite-3-value-numeric-type) :int (arg1 :pointer))
 (cffi:defcfun ("sqlite3_value_type" sqlite-3-value-type) :int (arg1 :pointer))
 (cffi:defcfun ("sqlite3_value_text16be" sqlite-3-value-text-16-be) :pointer (arg1 :pointer))
 (cffi:defcfun ("sqlite3_value_text16le" sqlite-3-value-text-16-le) :pointer (arg1 :pointer))
 (cffi:defcfun ("sqlite3_value_text16" sqlite-3-value-text-16) :pointer (arg1 :pointer))
 (cffi:defcfun ("sqlite3_value_text" sqlite-3-value-text) :pointer (arg1 :pointer))
 (cffi:defcfun ("sqlite3_value_int64" sqlite-3-value-int-64) sqlite-int-64 (arg1 :pointer))
 (cffi:defcfun ("sqlite3_value_int" sqlite-3-value-int) :int (arg1 :pointer))
 (cffi:defcfun ("sqlite3_value_double" sqlite-3-value-double) :double (arg1 :pointer))
 (cffi:defcfun ("sqlite3_value_bytes16" sqlite-3-value-bytes-16) :int (arg1 :pointer))
 (cffi:defcfun ("sqlite3_value_bytes" sqlite-3-value-bytes) :int (arg1 :pointer))
 (cffi:defcfun ("sqlite3_value_blob" sqlite-3-value-blob) :pointer (arg1 :pointer))
 (cffi:defcfun ("sqlite3_global_recover" sqlite-3-global-recover) :int)
 (cffi:defcfun ("sqlite3_transfer_bindings" sqlite-3-transfer-bindings) :int (arg1 :pointer)
  (arg2 :pointer))
 (cffi:defcfun ("sqlite3_expired" sqlite-3-expired) :int (arg1 :pointer))
 (cffi:defcfun ("sqlite3_aggregate_count" sqlite-3-aggregate-count) :int (arg1 :pointer))
 (cffi:defcfun ("sqlite3_create_function16" sqlite-3-create-function-16) :int (arg1 :pointer)
  (zFunctionName :pointer) (nArg :int) (eTextRep :int) (arg5 :pointer) (xFunc :pointer)
  (xStep :pointer) (xFinal :pointer))
 (cffi:defcfun ("sqlite3_create_function" sqlite-3-create-function) :int (arg1 :pointer)
  (zFunctionName :string) (nArg :int) (eTextRep :int) (arg5 :pointer) (xFunc :pointer)
  (xStep :pointer) (xFinal :pointer))
 (cffi:defcfun ("sqlite3_reset" sqlite-3-reset) :int (pStmt :pointer))
 (cffi:defcfun ("sqlite3_finalize" sqlite-3-finalize) :int (pStmt :pointer))
 (cffi:defcfun ("sqlite3_column_value" sqlite-3-column-value) :pointer (arg1 :pointer) (iCol :int))
 (cffi:defcfun ("sqlite3_column_type" sqlite-3-column-type) :int (arg1 :pointer) (iCol :int))
 (cffi:defcfun ("sqlite3_column_text16" sqlite-3-column-text-16) :pointer (arg1 :pointer)
  (iCol :int))
 (cffi:defcfun ("sqlite3_column_text" sqlite-3-column-text) :pointer (arg1 :pointer) (iCol :int))
 (cffi:defcfun ("sqlite3_column_int64" sqlite-3-column-int-64) sqlite-int-64 (arg1 :pointer)
  (iCol :int))
 (cffi:defcfun ("sqlite3_column_int" sqlite-3-column-int) :int (arg1 :pointer) (iCol :int))
 (cffi:defcfun ("sqlite3_column_double" sqlite-3-column-double) :double (arg1 :pointer)
  (iCol :int))
 (cffi:defcfun ("sqlite3_column_bytes16" sqlite-3-column-bytes-16) :int (arg1 :pointer)
  (iCol :int))
 (cffi:defcfun ("sqlite3_column_bytes" sqlite-3-column-bytes) :int (arg1 :pointer) (iCol :int))
 (cffi:defcfun ("sqlite3_column_blob" sqlite-3-column-blob) :pointer (arg1 :pointer) (iCol :int))
 (cffi:defcfun ("sqlite3_data_count" sqlite-3-data-count) :int (pStmt :pointer))
 (cffi:defcfun ("sqlite3_step" sqlite-3-step) :int (arg1 :pointer))
 (cffi:defcfun ("sqlite3_column_decltype16" sqlite-3-column-decltype-16) :pointer (arg1 :pointer)
  (arg2 :int))
 (cffi:defcfun ("sqlite3_column_decltype" sqlite-3-column-decltype) :string (arg1 :pointer)
  (i :int))
 (cffi:defcfun ("sqlite3_column_origin_name16" sqlite-3-column-origin-name-16) :pointer
  (arg1 :pointer) (arg2 :int))
 (cffi:defcfun ("sqlite3_column_origin_name" sqlite-3-column-origin-name) :string (arg1 :pointer)
  (arg2 :int))
 (cffi:defcfun ("sqlite3_column_table_name16" sqlite-3-column-table-name-16) :pointer
  (arg1 :pointer) (arg2 :int))
 (cffi:defcfun ("sqlite3_column_table_name" sqlite-3-column-table-name) :string (arg1 :pointer)
  (arg2 :int))
 (cffi:defcfun ("sqlite3_column_database_name16" sqlite-3-column-database-name-16) :pointer
  (arg1 :pointer) (arg2 :int))
 (cffi:defcfun ("sqlite3_column_database_name" sqlite-3-column-database-name) :string
  (arg1 :pointer) (arg2 :int))
 (cffi:defcfun ("sqlite3_column_name16" sqlite-3-column-name-16) :pointer (arg1 :pointer) (N :int))
 (cffi:defcfun ("sqlite3_column_name" sqlite-3-column-name) :string (arg1 :pointer) (N :int))
 (cffi:defcfun ("sqlite3_column_count" sqlite-3-column-count) :int (pStmt :pointer))
 (cffi:defcfun ("sqlite3_clear_bindings" sqlite-3-clear-bindings) :int (arg1 :pointer))
 (cffi:defcfun ("sqlite3_bind_parameter_index" sqlite-3-bind-parameter-index) :int (arg1 :pointer)
  (zName :string))
 (cffi:defcfun ("sqlite3_bind_parameter_name" sqlite-3-bind-parameter-name) :string (arg1 :pointer)
  (arg2 :int))
 (cffi:defcfun ("sqlite3_bind_parameter_count" sqlite-3-bind-parameter-count) :int (arg1 :pointer))
 (cffi:defcfun ("sqlite3_bind_zeroblob" sqlite-3-bind-zeroblob) :int (arg1 :pointer) (arg2 :int)
  (n :int))
 (cffi:defcfun ("sqlite3_bind_value" sqlite-3-bind-value) :int (arg1 :pointer) (arg2 :int)
  (arg3 :pointer))
 (cffi:defcfun ("sqlite3_bind_text16" sqlite-3-bind-text-16) :int (arg1 :pointer) (arg2 :int)
  (arg3 :pointer) (arg4 :int) (arg5 :pointer))
 (cffi:defcfun ("sqlite3_bind_text" sqlite-3-bind-text) :int (arg1 :pointer) (arg2 :int)
  (arg3 :string) (n :int) (arg5 :pointer))
 (cffi:defcfun ("sqlite3_bind_null" sqlite-3-bind-null) :int (arg1 :pointer) (arg2 :int))
 (cffi:defcfun ("sqlite3_bind_int64" sqlite-3-bind-int-64) :int (arg1 :pointer) (arg2 :int)
  (arg3 sqlite-int-64))
 (cffi:defcfun ("sqlite3_bind_int" sqlite-3-bind-int) :int (arg1 :pointer) (arg2 :int) (arg3 :int))
 (cffi:defcfun ("sqlite3_bind_double" sqlite-3-bind-double) :int (arg1 :pointer) (arg2 :int)
  (arg3 :double))
 (cffi:defcfun ("sqlite3_bind_blob" sqlite-3-bind-blob) :int (arg1 :pointer) (arg2 :int)
  (arg3 :pointer) (n :int) (arg5 :pointer))
 (cffi:defcfun ("sqlite3_prepare16_v2" sqlite-3-prepare-16-v-2) :int (db :pointer) (zSql :pointer)
  (nByte :int) (ppStmt :pointer) (pzTail :pointer))
 (cffi:defcfun ("sqlite3_prepare16" sqlite-3-prepare-16) :int (db :pointer) (zSql :pointer)
  (nByte :int) (ppStmt :pointer) (pzTail :pointer))
 (cffi:defcfun ("sqlite3_prepare_v2" sqlite-3-prepare-v-2) :int (db :pointer) (zSql :string)
  (nByte :int) (ppStmt :pointer) (pzTail :pointer))
 (cffi:defcfun ("sqlite3_prepare" sqlite-3-prepare) :int (db :pointer) (zSql :string) (nByte :int)
  (ppStmt :pointer) (pzTail :pointer))
 (cffi:defcfun ("sqlite3_errmsg16" sqlite-3-errmsg-16) :pointer (arg1 :pointer))
 (cffi:defcfun ("sqlite3_errmsg" sqlite-3-errmsg) :string (arg1 :pointer))
 (cffi:defcfun ("sqlite3_errcode" sqlite-3-errcode) :int (db :pointer))
 (cffi:defcfun ("sqlite3_open16" sqlite-3-open-16) :int (filename :pointer) (ppDb :pointer))
 (cffi:defcfun ("sqlite3_open" sqlite-3-open) :int (filename :string) (ppDb :pointer))
 (cffi:defcfun ("sqlite3_progress_handler" sqlite-3-progress-handler) :void (arg1 :pointer)
  (arg2 :int) (arg3 :pointer) (arg4 :pointer))
 (cffi:defcfun ("sqlite3_profile" sqlite-3-profile) :pointer (arg1 :pointer) (xProfile :pointer)
  (arg3 :pointer))
 (cffi:defcfun ("sqlite3_trace" sqlite-3-trace) :pointer (arg1 :pointer) (xTrace :pointer)
  (arg3 :pointer))
 (cffi:defcfun ("sqlite3_set_authorizer" sqlite-3-set-authorizer) :int (arg1 :pointer)
  (xAuth :pointer) (pUserData :pointer))
 (cffi:defcfun ("sqlite3_free" sqlite-3-free) :void (arg1 :pointer))
 (cffi:defcfun ("sqlite3_realloc" sqlite-3-realloc) :pointer (arg1 :pointer) (arg2 :int))
 (cffi:defcfun ("sqlite3_malloc" sqlite-3-malloc) :pointer (arg1 :int))
 (cffi:defcfun ("sqlite3_snprintf" sqlite-3-snprintf) :pointer (arg1 :int) (arg2 :pointer)
  (arg3 :string))
 (cffi:defcfun ("sqlite3_vmprintf" sqlite-3-vmprintf) :pointer (arg1 :string) (arg2 :pointer))
 (cffi:defcfun ("sqlite3_mprintf" sqlite-3-mprintf) :pointer (arg1 :string))
 (cffi:defcfun ("sqlite3_free_table" sqlite-3-free-table) :void (result :pointer))
 (cffi:defcfun ("sqlite3_get_table" sqlite-3-get-table) :int (arg1 :pointer) (sql :string)
  (resultp :pointer) (nrow :pointer) (ncolumn :pointer) (errmsg :pointer))
 (cffi:defcfun ("sqlite3_busy_timeout" sqlite-3-busy-timeout) :int (arg1 :pointer) (ms :int))
 (cffi:defcfun ("sqlite3_busy_handler" sqlite-3-busy-handler) :int (arg1 :pointer) (arg2 :pointer)
  (arg3 :pointer))
 (cffi:defcfun ("sqlite3_complete16" sqlite-3-complete-16) :int (sql :pointer))
 (cffi:defcfun ("sqlite3_complete" sqlite-3-complete) :int (sql :string))
 (cffi:defcfun ("sqlite3_interrupt" sqlite-3-interrupt) :void (arg1 :pointer))
 (cffi:defcfun ("sqlite3_total_changes" sqlite-3-total-changes) :int (arg1 :pointer))
 (cffi:defcfun ("sqlite3_changes" sqlite-3-changes) :int (arg1 :pointer))
 (cffi:defcfun ("sqlite3_last_insert_rowid" sqlite-3-last-insert-rowid) sqlite-int-64
  (arg1 :pointer))
 (cffi:defcfun ("sqlite3_extended_result_codes" sqlite-3-extended-result-codes) :int
  (arg1 :pointer) (onoff :int))
 (cffi:defcfun ("sqlite3_exec" sqlite-3-exec) :int (arg1 :pointer) (sql :string)
  (callback :pointer) (arg4 :pointer) (errmsg :pointer))
 (cffi:defcfun ("sqlite3_close" sqlite-3-close) :int (arg1 :pointer))
 (cffi:defcfun ("sqlite3_libversion_number" sqlite-3-libversion-number) :int)
 (cffi:defcfun ("sqlite3_libversion" sqlite-3-libversion) :string)
 (cl:defconstant +sqlite-version-number+ 3004002) (cl:defconstant +sqlite-ok+ 0)
 (cl:defconstant +sqlite-error+ 1) (cl:defconstant +sqlite-internal+ 2)
 (cl:defconstant +sqlite-perm+ 3) (cl:defconstant +sqlite-abort+ 4)
 (cl:defconstant +sqlite-busy+ 5) (cl:defconstant +sqlite-locked+ 6)
 (cl:defconstant +sqlite-nomem+ 7) (cl:defconstant +sqlite-readonly+ 8)
 (cl:defconstant +sqlite-interrupt+ 9) (cl:defconstant +sqlite-ioerr+ 10)
 (cl:defconstant +sqlite-corrupt+ 11) (cl:defconstant +sqlite-notfound+ 12)
 (cl:defconstant +sqlite-full+ 13) (cl:defconstant +sqlite-cantopen+ 14)
 (cl:defconstant +sqlite-protocol+ 15) (cl:defconstant +sqlite-empty+ 16)
 (cl:defconstant +sqlite-schema+ 17) (cl:defconstant +sqlite-toobig+ 18)
 (cl:defconstant +sqlite-constraint+ 19) (cl:defconstant +sqlite-mismatch+ 20)
 (cl:defconstant +sqlite-misuse+ 21) (cl:defconstant +sqlite-nolfs+ 22)
 (cl:defconstant +sqlite-auth+ 23) (cl:defconstant +sqlite-format+ 24)
 (cl:defconstant +sqlite-range+ 25) (cl:defconstant +sqlite-notadb+ 26)
 (cl:defconstant +sqlite-row+ 100) (cl:defconstant +sqlite-done+ 101)
 (cl:defconstant +sqlite-deny+ 1) (cl:defconstant +sqlite-ignore+ 2)
 (cl:defconstant +sqlite-create-index+ 1) (cl:defconstant +sqlite-create-table+ 2)
 (cl:defconstant +sqlite-create-temp-index+ 3) (cl:defconstant +sqlite-create-temp-table+ 4)
 (cl:defconstant +sqlite-create-temp-trigger+ 5) (cl:defconstant +sqlite-create-temp-view+ 6)
 (cl:defconstant +sqlite-create-trigger+ 7) (cl:defconstant +sqlite-create-view+ 8)
 (cl:defconstant +sqlite-delete+ 9) (cl:defconstant +sqlite-drop-index+ 10)
 (cl:defconstant +sqlite-drop-table+ 11) (cl:defconstant +sqlite-drop-temp-index+ 12)
 (cl:defconstant +sqlite-drop-temp-table+ 13) (cl:defconstant +sqlite-drop-temp-trigger+ 14)
 (cl:defconstant +sqlite-drop-temp-view+ 15) (cl:defconstant +sqlite-drop-trigger+ 16)
 (cl:defconstant +sqlite-drop-view+ 17) (cl:defconstant +sqlite-insert+ 18)
 (cl:defconstant +sqlite-pragma+ 19) (cl:defconstant +sqlite-read+ 20)
 (cl:defconstant +sqlite-select+ 21) (cl:defconstant +sqlite-transaction+ 22)
 (cl:defconstant +sqlite-update+ 23) (cl:defconstant +sqlite-attach+ 24)
 (cl:defconstant +sqlite-detach+ 25) (cl:defconstant +sqlite-alter-table+ 26)
 (cl:defconstant +sqlite-reindex+ 27) (cl:defconstant +sqlite-analyze+ 28)
 (cl:defconstant +sqlite-create-vtable+ 29) (cl:defconstant +sqlite-drop-vtable+ 30)
 (cl:defconstant +sqlite-function+ 31) (cl:defconstant +sqlite-copy+ 0)
 (cl:defconstant +sqlite-integer+ 1) (cl:defconstant +sqlite-float+ 2)
 (cl:defconstant +sqlite-blob+ 4) (cl:defconstant +sqlite-null+ 5) (cl:defconstant +sqlite-text+ 3)
 (cl:defconstant +sqlite-3-text+ 3) (cl:defconstant +sqlite-utf-8+ 1)
 (cl:defconstant +sqlite-utf-16-le+ 2) (cl:defconstant +sqlite-utf-16-be+ 3)
 (cl:defconstant +sqlite-utf-16+ 4) (cl:defconstant +sqlite-any+ 5)
 (cl:defconstant +sqlite-utf-16-aligned+ 8) (cl:defconstant +sqlite-index-constraint-eq+ 2)
 (cl:defconstant +sqlite-index-constraint-gt+ 4) (cl:defconstant +sqlite-index-constraint-le+ 8)
 (cl:defconstant +sqlite-index-constraint-lt+ 16) (cl:defconstant +sqlite-index-constraint-ge+ 32)
 (cl:defconstant +sqlite-index-constraint-match+ 64))
