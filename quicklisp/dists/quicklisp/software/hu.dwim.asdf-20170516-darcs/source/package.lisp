;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.asdf
  (:use :common-lisp)
  (:import-from :asdf
                #:find-system
                #:initialize-source-registry
                #:compile-op
                #:load-op
                #:load-system
                #:non-propagating-operation
                #:perform
                #:system-relative-pathname
                #:test-op
                #:test-system)
  (:export #:find-and-load-swank-integration-systems
           #:hu.dwim.system
           #:hu.dwim.test-system
           #:hu.dwim.documentation-system
           #:system-pathname
           #:system-directory
           #:system-relative-pathname
           #:system-package-name
           #:system-test-name
           #:system-test-system-name
           #:system-documentation-system-name
           #:system-compile-output
           #:system-load-output
           #:system-test-result
           #:system-test-output
           #:develop-op
           #:develop-system
           #:*load-as-production?*
           #:debug-only
           #:debug-only*
           #:production-only
           #:production-only*
           #:optimize-declaration
           #:iterate-system-dependencies
           #:map-asdf-source-registry-directories
           #:do-system-dependencies
           #:map-system-dependencies
           #:collect-system-dependencies
           #:find-system
           #:load-system
           #:test-system
           #:run-test-suite))
