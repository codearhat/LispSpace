;;; -*- Mode: Lisp -*-

(asdf:defsystem "plain-odbc"
  :components (
               (:module "odbc-stuff"
                        :pathname "src/odbc/"
                        :components 
                        ((:file "plain-odbc-package")
                         (:file "odbc-constants")
                         (:file "global")
                         (:file "ffi-support")
                         (:file "odbc-ff-interface")
                         (:file "odbc-functions")
                         (:file "parameter")
                         (:file "column")
                         (:file "odbc-main")
                         (:file "odbc-utilities"))
                        :serial t
                        ))
  :serial t
  :depends-on (:cffi))
