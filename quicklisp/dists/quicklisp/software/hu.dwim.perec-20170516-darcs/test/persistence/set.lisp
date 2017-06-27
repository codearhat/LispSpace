;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/persistence/set :in test/persistence))

(def persistent-class* referred-set-test ()
  ())

(def persistent-class* reference-set-test ()
  ((referred-set :type (set referred-set-test))))

(def macro with-reference-set-transaction (&body body)
  `(with-transaction
    (let ((referred (make-instance 'referred-set-test))
          (reference-set (make-instance 'reference-set-test)))
      (declare (ignorable referred reference-set))
      ,@body)))

(def test test/persistence/set/class ()
  (let ((referred-set-slot (find-slot 'reference-set-test 'referred-set)))
    (is (not (primary-table-slot-p referred-set-slot)))
    (is (not (data-table-slot-p referred-set-slot)))
    (is (not (cache-p referred-set-slot)))))

(def test test/persistence/set/initial-value/1 ()
  (with-reference-set-transaction
    (is (eq nil (referred-set-of reference-set)))))

(def test test/persistence/set/initial-value/2 ()
  (with-transaction
    (bind ((referred (make-instance 'referred-set-test))
           (reference-set (make-instance 'reference-set-test :referred-set (list referred))))
      (is (equal (referred-set-of reference-set) (list referred))))))

(def test test/persistence/set/store-value/1 ()
  (with-reference-set-transaction
    (setf (referred-set-of reference-set) (list referred))
    (is (equal (list referred) (referred-set-of reference-set)))))

(def test test/persistence/set/collection/1 ()
  (with-reference-set-transaction
    (bind ((referred-set (referred-set-of* reference-set)))
      (insert-item referred-set referred)
      (is (= 1 (size referred-set)))
      (is (equal (list referred) (referred-set-of reference-set)))
      (delete-item referred-set referred)
      (is (= 0 (size referred-set)))
      (is (null (referred-set-of reference-set))))))

(def test test/persistence/set/collection/2 ()
  (with-reference-set-transaction
    (bind ((referred-set (referred-set-of* reference-set))
           (other-referred (make-instance 'referred-set-test)))
      (insert-item referred-set referred)
      (insert-item referred-set other-referred)
      (delete-item referred-set referred)
      (is (= 1 (size referred-set)))
      (is (equal (list other-referred) (list-of referred-set))))))
