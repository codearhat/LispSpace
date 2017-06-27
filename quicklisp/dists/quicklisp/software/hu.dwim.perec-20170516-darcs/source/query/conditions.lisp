;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

(defcondition* query-error (error)
  ())

(defcondition* query-syntax-error (query-error)
  ((form)
   (detail nil))
  (:report (lambda (condition stream)
             (with-slots (form detail) condition
                 (format stream "Syntax error~@[ (~A)~]:~%~%In query:~%~W"
                         detail form)))))

(defcondition* missing-query-clause-error (query-syntax-error)
  ((clause))
  (:report (lambda (condition stream)
             (with-slots (form clause) condition
               (format stream "Missing ~:@(~A~) clause:~%~%In query:~%~W"
                       clause form)))))

(def function missing-query-clause-error (form clause &optional detail)
  (error 'missing-query-clause-error :form form :clause clause :detail detail))

(defcondition* unrecognized-query-clause-error (query-syntax-error)
  ((clause))
  (:report (lambda (condition stream)
             (with-slots (form clause) condition
               (format stream "Unrecognized clause (~:@(~A~)):~%~%In query:~%~W"
                       clause form)))))

(def function unrecognized-query-clause-error (form clause &optional detail)
  (error 'unrecognized-query-clause-error :form form :clause clause :detail detail))

(defcondition* duplicated-query-clause-error (query-syntax-error)
  ((clause))
  (:report (lambda (condition stream)
             (with-slots (form clause) condition
               (format stream "Duplicated ~:@(~A~) clause:~%~%In query:~%~W"
                       clause form)))))

(def function duplicated-query-clause-error (form clause &optional detail)
  (error 'duplicated-query-clause-error :form form :clause clause :detail detail))

(defcondition* malformed-query-clause-error (query-syntax-error)
  ((clause-form))
  (:report (lambda (condition stream)
             (with-slots (form detail clause-form) condition
               (format stream "Malformed clause~@[ (~A)~]: ~S~%~%In query:~%~W"
                       detail clause-form form)))))

(def function malformed-query-clause-error (form clause-form &optional detail)
  (error 'malformed-query-clause-error :form form :clause-form clause-form :detail detail))

(defcondition* query-runtime-error (query-error)
  ((query)))

;; FIXME this should be in the test suite...
(defcondition* query-result-mismatch-error (query-runtime-error)
  ((result)
   (expected))
  (:documentation "Condition signalling that the runtime check of the query failed.")
  (:report (lambda (condition stream)
             (with-slots (query result expected) condition
               #+nil ; only when it's moved into the test suite
               (format stream "Query failed~:[~:; (expected)~]:~&~S. Result is ~:W, but expected ~:W."
                       hu.dwim.stefil::*failures-and-errors-are-expected* query result expected)
               (format stream "Query ~S failed. Result is ~:W, but expected ~:W."
                       query result expected)))))

(defcondition* query-warning (warning)
  ())

(defcondition* query-compiler-warning (query-warning)
  ())

(defcondition* slot-not-found-warning (query-compiler-warning)
  ((accessor)
   (arg-type)
   (access-type))
  (:documentation "Warning signalling that the query compiler cannot find the slot for a slot access.")
  (:report (lambda (condition stream)
             (with-slots (accessor arg-type access-type) condition
                 (format stream "No slot found with type <~A> for slot access (~A <~A>)."
                         accessor arg-type access-type)))))

(defcondition* ambiguous-slot-warning (query-compiler-warning)
  ((accessor)
   (arg-type)
   (access-type)
   (slot-names))
  (:documentation "Warning signalling that the query compiler cannot identify the slot for a slot access.")
  (:report (lambda (condition stream)
             (with-slots (accessor arg-type access-type slot-names) condition
               (format stream "More slots found with type <~A> for slot access (~A <~A>): ~A."
                       access-type accessor arg-type slot-names)))))
