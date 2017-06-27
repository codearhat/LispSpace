;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def special-variable *association-1-n-parent-class-name* 'parent-test)

(def special-variable *association-1-n-child-class-name* 'child-test)

(def suite* (test/persistence/association/1-n :in test/persistence/association) ()
  (flet ((body ()
           (with-and-without-caching-slot-values
             (-run-child-tests-))))
    (body)
    (bind ((*association-1-n-parent-class-name* '1-n-self-association-test)
           (*association-1-n-child-class-name* '1-n-self-association-test))
      (body))
    (bind ((*association-1-n-parent-class-name* 'concrete-parent-test)
           (*association-1-n-child-class-name* 'concrete-child-test))
      (body))))

(def persistent-class* parent-test ()
  ())
   
(def persistent-class* child-test ()
  ())

(def persistent-class* 1-n-self-association-test ()
  ())

(def persistent-association*
  ((:class 1-n-self-association-test :slot children :type (set 1-n-self-association-test))
   (:class 1-n-self-association-test :slot parent :type (or null 1-n-self-association-test))))

(def persistent-association*
  ((:class child-test :slot parent :type (or null parent-test))
   (:class parent-test :slot children :type (set child-test))))

(def persistent-class abstract-parent-test ()
  ()
  (:abstract #t)
  (:direct-store :push-down))

(def persistent-class concrete-parent-test (abstract-parent-test)
  ())

(def persistent-class abstract-child-test ()
  ()
  (:abstract #t)
  (:direct-store :push-down))

(def persistent-class concrete-child-test (abstract-child-test)
  ())

(def persistent-association*
  ((:class abstract-child-test :slot parent :type (or null abstract-parent-test))
   (:class abstract-parent-test :slot children :type (set abstract-child-test))))

(def macro with-parent-and-child-transaction (&body body)
  `(with-transaction
    (bind ((parent (make-instance *association-1-n-parent-class-name*))
           (child (make-instance *association-1-n-child-class-name*)))
      ,@body)))

(def test test/persistence/association/1-n/class ()
  (ensure-exported (find-class *association-1-n-child-class-name*))
  (ensure-exported (find-class *association-1-n-parent-class-name*))
  (let ((parent-slot (find-slot *association-1-n-child-class-name* 'parent))
        (children-slot (find-slot *association-1-n-parent-class-name* 'children)))
    (is (primary-table-slot-p parent-slot))
    (is (data-table-slot-p parent-slot))
    (is (not (primary-table-slot-p children-slot)))
    (is (not (data-table-slot-p children-slot)))
    (is (cache-p parent-slot))
    (is (not (cache-p children-slot)))))

(def test test/persistence/association/1-n/initial-value/1 ()
  (with-parent-and-child-transaction
    (is (eq nil (parent-of child)))
    (is (= 0 (size (children-of parent))))))
   
(def test test/persistence/association/1-n/initial-value/2 ()
  (with-transaction
    (bind ((parent (make-instance *association-1-n-parent-class-name*))
           (child (make-instance *association-1-n-child-class-name* :parent parent)))
      (is (eq (parent-of child) parent)))))

(def test test/persistence/association/1-n/initial-value/3 ()
  (with-transaction
    (bind ((child (make-instance *association-1-n-child-class-name*))
           (parent (make-instance *association-1-n-parent-class-name* :children (list child))))
      (is (equal (children-of parent) (list child))))))

(def test test/persistence/association/1-n/store-value/1 ()
  (with-parent-and-child-transaction
    (setf (parent-of child) parent)
    (is (eq parent (parent-of child)))))

(def test test/persistence/association/1-n/store-value/2 ()
  (with-parent-and-child-transaction
    (setf (children-of parent) (list child))
    (is (equal (list child) (children-of parent)))))

(def test test/persistence/association/1-n/referential-integrity/1 ()
  (with-parent-and-child-transaction
    (setf (parent-of child) parent)
    (bind ((children (children-of parent)))
      (is (= 1 (size children)))
      (is (equal (list child) children)))))

(def test test/persistence/association/1-n/referential-integrity/2 ()
  (with-parent-and-child-transaction
    (setf (children-of parent) (list child))
    (is (eq parent (parent-of child)))))

(def test test/persistence/association/1-n/referential-integrity/3 ()
  (with-parent-and-child-transaction
    (setf (children-of parent) (list child))
    (setf (parent-of child) (make-instance *association-1-n-parent-class-name*))
    (is (= 0 (size (children-of parent))))))

(def test test/persistence/association/1-n/referential-integrity/4 ()
  (with-parent-and-child-transaction
    (setf (parent-of child) parent)
    (setf (children-of parent) (list (make-instance *association-1-n-child-class-name*)))
    (is (eq nil (parent-of child)))))

(def test test/persistence/association/1-n/collection/1 ()
  (with-parent-and-child-transaction
    (bind ((children (children-of* parent)))
      (insert-item children child)
      (is (= 1 (size children)))
      (is (equal (list child) (children-of parent)))
      (delete-item children child)
      (is (= 0 (size children)))
      (is (null (children-of parent))))))

(def test test/persistence/association/1-n/collection/2 ()
  (with-parent-and-child-transaction
    (bind ((children (children-of* parent)))
      (insert-item children child)
      (ensure-item children child)
      (signals error (insert-item children child)))))

(def test test/persistence/association/1-n/collection/3 ()
  (with-parent-and-child-transaction
    (bind ((children (children-of* parent))
           (other-child (make-instance *association-1-n-child-class-name*)))
      (insert-item children child)
      (insert-item children other-child)
      (delete-item children child)
      (is (= 1 (size children)))
      (is (equal (list other-child) (list-of children))))))

(def test test/persistence/association/1-n/collection/4 ()
  (with-parent-and-child-transaction
    (bind ((children (children-of* parent)))
      (insert-item children child)
      (delete-item children child)
      (signals error (delete-item children child)))))

(def test test/persistence/association/1-n/collection/5 ()
  (with-parent-and-child-transaction
    (bind ((children (children-of* parent))
           (other-child (make-instance *association-1-n-child-class-name*)))
      (insert-item children child)
      (is (find-item children child))
      (is (not (find-item children other-child))))))
