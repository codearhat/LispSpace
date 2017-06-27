;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.util
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Various utilities, this is the most basic system that only introduce a small number of external dependencies."
  :depends-on (:hu.dwim.def+hu.dwim.common
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.syntax-sugar
               ;; TODO at the time of writing the sb-sprof contrib is broken on windows.
               ;; see https://bugs.launchpad.net/sbcl/+bug/1274943
               ;; see also below
               #+(and sbcl (not windows)) :sb-sprof)
  :components ((:module "source"
                :components ((:file "anaphora" :depends-on ("package"))
                             (:file "dynamic-context" :depends-on ("miscellaneous"))
                             (:file "error-handling-early" :depends-on ("package" "miscellaneous"))
                             (:file "generic-operator" :depends-on ("package"))
                             (:file "hash-table" :depends-on ("package"))
                             (:file "integer-to-string" :depends-on ("package"))
                             (:file "iterate" :depends-on ("package"))
                             (:file "number" :depends-on ("package"))
                             (:file "package")
                             (:file "pattern-matcher" :depends-on ("package"))
                             (:file "place" :depends-on ("package"))
                             (:file "sequence" :depends-on ("package"))
                             (:file "string" :depends-on ("miscellaneous"))
                             (:file "string-early" :depends-on ("package"))
                             (:file "threads-early" :depends-on ("package"))
                             (:file "type" :depends-on ("package"))
                             (:file "miscellaneous" :depends-on ("package" "string-early"))))
                (:module "integration"
                 :components ((:file "sbcl" :if-feature (:and :sbcl (:not :windows)))))))

(defsystem :hu.dwim.util/authorization
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Authorization for lisp forms."
  :depends-on (:hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.logger
               :hu.dwim.partial-eval
               :hu.dwim.util
               :hu.dwim.walker)
  :components ((:module "source"
                :components ((:file "authorization")))))

(defsystem :hu.dwim.util/command-line
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :depends-on (:hu.dwim.util
               :command-line-arguments
               :uiop)
  :components ((:module "source"
                :components ((:file "command-line")))))

(defsystem :hu.dwim.util/error-handling
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Various utilities, contains code for complex error handling."
  :depends-on (:hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.util)
  :components ((:module "source"
                :components ((:file "error-handling")))
               (:module "integration"
                :components ((:file "backtrace-sbcl" :if-feature :sbcl)))))

(defsystem :hu.dwim.util/error-handling+swank
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :depends-on (:hu.dwim.util/error-handling
               :swank)
  :components ((:module "integration"
                :components ((:file "error-handling+swank")))))

(defsystem :hu.dwim.util/finite-state-machine
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description ""
  :depends-on (:hu.dwim.def.namespace
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.util)
  :components ((:module "source"
                :components ((:file "finite-state-machine")))))

(defsystem :hu.dwim.util/flexml
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "A CXML document model that can parse into CLOS nodes"
  :depends-on (:cl-ppcre
               :cxml
               :hu.dwim.def
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.def.namespace
               :hu.dwim.util)
  :components ((:module "source"
                :components ((:file "flexml" :depends-on ("flexml-package"))
                             (:file "flexml-package")))))

(defsystem :hu.dwim.util/i18n
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :depends-on (:hu.dwim.util
               :cl-l10n)
  :components ((:module "source"
                :components ((:file "i18n")))))

(defsystem :hu.dwim.util/linear-mapping
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description ""
  :depends-on (:bordeaux-threads
               :hu.dwim.util)
  :components ((:module "source"
                :components ((:file "linear-mapping")))))

(defsystem :hu.dwim.util/mop
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :depends-on (:hu.dwim.util
               :closer-mop)
  :components ((:module "source"
                :components ((:file "mop")))))

(defsystem :hu.dwim.util/production
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Various utilities, contains code for producing standalone executable services."
  :depends-on (:hu.dwim.logger
               :hu.dwim.perec.postgresql     ; TODO drop dependency
               :hu.dwim.util/command-line
               :hu.dwim.util/error-handling+swank
               :hu.dwim.util/temporary-files
               :hu.dwim.util+iolib
               :hu.dwim.web-server.application ; TODO drop dependency
               :iolib/syscalls)
  :components ((:module "source"
                :components ((:file "production")))))

(defsystem :hu.dwim.util/soap
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "SOAP messages over HTTP."
  :depends-on (:babel
               :babel-streams
               :cxml
               :drakma
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.logger
               :hu.dwim.quasi-quote.xml
               :hu.dwim.util
               :hu.dwim.util/flexml)
  :components ((:module "source"
                :components ((:file "soap")))))

(defsystem :hu.dwim.util/source
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Infrastructure to provide the lisp source forms for named global definitions. SBCL only at this point."
  :depends-on (:hu.dwim.def+hu.dwim.common
               :hu.dwim.syntax-sugar
               :hu.dwim.util
               :swank)
  :components ((:module "source"
                :components ((:file "source")))))

(defsystem :hu.dwim.util/standard-process
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Provides a worker group abstraction to do a bunch of shared tasks."
  :depends-on (:hu.dwim.delico
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.logger
               :hu.dwim.util/finite-state-machine)
  :components ((:module "source"
                :components ((:file "standard-process")))))

(defsystem :hu.dwim.util/temporary-files
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description ""
  :depends-on (:hu.dwim.util+iolib
               :iolib/pathnames
               :iolib/os
               :iolib/syscalls)
  :components ((:module "source"
                :components ((:file "temporary-files")))))

(defsystem :hu.dwim.util/threads
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Various utilities used by the dwim.hu team. Threading related utilities for a bit more dependency."
  :depends-on (:bordeaux-threads
               :hu.dwim.def.namespace
               :hu.dwim.util)
  :components ((:module "source"
                :components ((:file "threads")))))

(defsystem :hu.dwim.util/worker-group
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Provides a worker group abstraction to do a bunch of shared tasks."
  :depends-on (:bordeaux-threads
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.logger
               :hu.dwim.util/error-handling)
  :components ((:module "source"
                :components ((:file "worker-group")))))

(defsystem :hu.dwim.util/zlib
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Bindings and lisp API for zlib."
  :depends-on (:cffi
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.logger
               :hu.dwim.util)
  :components ((:module "source"
                :components ((:file "zlib")))))
