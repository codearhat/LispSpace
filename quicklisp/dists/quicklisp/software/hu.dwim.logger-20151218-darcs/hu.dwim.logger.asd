;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.logger
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Generic purpose logger utility."
  :depends-on (:bordeaux-threads
               :hu.dwim.def.namespace
               :hu.dwim.def+hu.dwim.common
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.util
               :hu.dwim.util/threads
               :local-time)
  :components ((:module "source"
                :components ((:file "api" :depends-on ("package"))
                             (:file "appender" :depends-on ("api" "formatter" "logger"))
                             (:file "formatter" :depends-on ("api" "logger"))
                             (:file "logger" :depends-on ("api"))
                             (:file "package")
                             (:file "root-logger" :depends-on ("logger" "api" "appender"))))))
