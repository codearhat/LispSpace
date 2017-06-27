;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/dimensional/cache :in test/dimensional))

(def persistent-class* dimensional-cache-test ()
  ((time-dependent-int :type integer :dimensions (time))
   (validity-dependent-string :type string :dimensions (validity))))

(def persistent-association*
  ((:class dimensional-cache-test :slot time-dependent-parent :type (or null dimensional-cache-test) :cache #t)
   (:class dimensional-cache-test :slot time-dependent-children :type (set dimensional-cache-test) :cache #t))
  (:dimensions (time)))

(def test test/dimensional/cache/tree ()
  (with-transaction
    (bind ((tree (create-dimensional-cache-test-tree))
           (select-counter (current-select-counter)))
      (cache-dimensional-slots (find-class 'dimensional-cache-test))
      (is (= (current-select-counter) (1+ select-counter)))
      (cache-dimensional-association
       (find-persistent-association
        'dimensional-cache-test~time-dependent-parent~dimensional-cache-test~time-dependent-children))
      (is (= (current-select-counter) (+ select-counter 2)))
      (walk-dimensional-cache-test-tree tree)
      (is (= (current-select-counter) (+ select-counter 2))))))

(def test test/dimensional/cache/tree/2 ()
  (with-transaction
    (create-dimensional-cache-test-tree)
    (bind ((root (ensure-cached-tree-d
                  'cache-test
                  'dimensional-cache-test
                  'dimensional-cache-test~time-dependent-parent~dimensional-cache-test~time-dependent-children))
           (select-counter (current-select-counter)))
      (walk-dimensional-cache-test-tree root)
      (is (= (current-select-counter) select-counter)))))

(def function create-dimensional-cache-test-tree ()
  (purge-instances 'dimensional-cache-test)
  (purge-instances 'dimensional-cache-test-h)
  (purge-instances 'dimensional-cache-test~time-dependent-parent~dimensional-cache-test~time-dependent-children)
  (labels ((recurse (parent depth index)
             (bind ((instance (make-instance 'dimensional-cache-test
                                             :time-dependent-int depth
                                             :validity-dependent-string (princ-to-string index)
                                             :time-dependent-parent parent)))
               (unless (zerop depth)
                 (iter (for i :from 0 :below 2)
                       (recurse instance (1- depth) i)))
               instance)))
    (recurse nil 3 0)))

(def function walk-dimensional-cache-test-tree (node)
  (bind ((int-value (time-dependent-int-of node))
         (string-value (validity-dependent-string-of node)))
    (declare (ignore int-value string-value))
    (iter (for child :in (single-d-value (time-dependent-children-of node)))
          (walk-dimensional-cache-test-tree child))))
