;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

(def (special-variable e) *compiled-query-cache*)

(def (with-macro e) with-compiled-query-cache (cache)
  (bind ((*compiled-query-cache* cache))
    (-body-)))

(def (macro e) with-new-compiled-query-cache (&body body)
  `(with-compiled-query-cache (make-compiled-query-cache)
     ,@body))

(def (function e) make-compiled-query-cache ()
  (make-hash-table :test #'equal))

(def (function e) clear-compiled-query-cache (&optional (cache *compiled-query-cache*))
  (clrhash cache))

(defun get-compiled-query (query)
  (if (boundp '*compiled-query-cache*)
      (let ((compiled-query (gethash (query-hash-key-for query) *compiled-query-cache*)))
        (when (not compiled-query)
          ;; TODO: the query is copied here because the caller can change it
          ;;       the change should remove the query from the cache instead
          (setf query (copy-query query))
          (setf compiled-query (compute-as* (:kind hu.dwim.computed-class::standalone) (eval (compile-query query))))
          (setf (gethash (query-hash-key-for query) *compiled-query-cache*) compiled-query))
        (computed-state-value compiled-query))
      (progn
        (warn "*COMPILED-QUERY-CACHE* is unbound, query compiler cache is disabled. See WITH-COMPILED-QUERY-CACHE and MAKE-COMPILED-QUERY-CACHE.")
        (eval (compile-query query)))))

(defmethod execute-query ((query query) &rest lexical-variable-values)
  (let ((compiled-query (get-compiled-query query)))
    (apply compiled-query lexical-variable-values)))

(defmethod execute-query ((query cons) &rest lexical-variable-values)
  (assert (null lexical-variable-values) () "Lexical vairables are not supported this way, use a nested MAKE-QUERY/EXECUTE-QUERY.")
  (execute-query (make-query query)))
