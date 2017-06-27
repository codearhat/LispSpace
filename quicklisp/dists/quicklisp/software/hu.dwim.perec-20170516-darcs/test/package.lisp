;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.perec.test
  (:use :cl-ppcre
        :hu.dwim.common
        :hu.dwim.computed-class
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.logger
        :hu.dwim.perec
        :hu.dwim.rdbms
        :hu.dwim.stefil
        :hu.dwim.util
        :local-time
        :metacopy-with-contextl)

  (:shadow #:name
           #:parent)

  (:shadowing-import-from :hu.dwim.perec
                          #:time
                          #:form
                          #:set)

  (:readtable-setup (setup-readtable/same-as-package :hu.dwim.perec)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import (let ((*package* (find-package :hu.dwim.perec)))
            (read-from-string "(*persistent-classes*
                                *persistent-associations* *cache-slot-values*
                                *mapped-type-precedence-list* *canonical-types* *compile-query-counter*
                                *test-query-compiler* +unbound-slot-marker+ +not-cached-slot-marker+
                                unbound-slot-marker-p
                                canonical-type-for normalized-type-for mapped-type-for
                                first-moment-for-partial-timestamp last-moment-for-partial-timestamp
                                less-or-equal-p greater-or-equal-p
                                validity-begin validity-end t-value unbound-slot-t
                                find-slot collect-if concatenate-symbol
                                unbound-subtype-p null-subtype-p set-type-p set-type-p*
                                invalidate-cached-instance invalidate-all-cached-instances persistent
                                clear-compiled-query-cache reset-compile-query-counter
                                ensure-exported primary-table-slot-p data-table-slot-p
                                primary-table-of primary-table-of data-tables-of
                                direct-instances-identity-view-of direct-instances-data-view-of
                                all-instances-identity-view-of all-instances-data-view-of
                                prefetch-p cache-p compute-mapping reader-of writer-of
                                compute-rdbms-types compute-reader compute-writer
                                table-of columns-of reader-name-of writer-name-of
                                lisp-value->rdbms-values rdbms-values->lisp-value
                                depends-on-of depends-on-me-of primary-class-of effective-store-of
                                compile-query)"))
          :hu.dwim.perec.test))
