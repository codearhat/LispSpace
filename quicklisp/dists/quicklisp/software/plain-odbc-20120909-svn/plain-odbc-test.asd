;;; -*- Mode: Lisp -*-



(asdf:defsystem "plain-odbc-test"
  :components (
               (:module "test-stuff"
                        :pathname "src/test/"
                        :components 
                        ((:file "test-main")
                         (:file "test-sql-server")
                         (:file "test-oracle")
                         (:file "test-mysql")
                         (:file "test-postgresql"))
                        :serial t
                        ))
  :serial t
  :depends-on (:plain-odbc))