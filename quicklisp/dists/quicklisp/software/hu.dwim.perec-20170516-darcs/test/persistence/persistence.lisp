;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(in-suite test/persistence)

(def persistent-class* persistence-test ()
  ((name :type (text 20))))

(def test test/persistence/make-instance/1 ()
  (with-transaction
    (is (persistent-p (make-instance 'persistence-test :name "the one")))))

(def test test/persistence/make-instance/2 ()
  (let ((instance
         (with-transaction
           (make-instance 'persistence-test :name "the one"))))
    (with-transaction
      (revive-instance instance)
      (is (persistent-p instance)))))

(def test test/persistence/make-transient/1 ()
  (let ((instance (make-instance 'persistence-test :name "the one" :persistent #f)))
    (is (not (persistent-p instance)))))

(def test test/persistence/make-transient/2 ()
  (with-transaction
    (let ((instance (make-instance 'persistence-test :name "the one")))
      (make-transient instance)
      (is (not (persistent-p instance))))))

(def test test/persistence/make-transient/3 ()
  (with-transaction
    (let ((instance (make-instance 'persistence-test :name "the one")))
      (delete-records (hu.dwim.rdbms::sql-table-alias :name (rdbms-name-for 'persistence-test)))
      (slot-makunbound instance 'persistent)
      (is (not (persistent-p instance))))))

(def test test/persistence/make-transient/4 ()
  (let ((instance
         (with-transaction
           (make-instance 'persistence-test :name "the one"))))
    (with-transaction
      (with-revived-instance instance
        (hu.dwim.perec::invalidate-cached-slot instance (find-slot 'persistence-test 'name))
        (make-transient instance)
        (is (equal (name-of instance) "the one"))))))
  
(def test test/persistence/make-persistent/1 ()
  (with-transaction
    (let ((instance (make-instance 'persistence-test :name "the one")))
      (make-transient instance)
      (make-persistent instance)
      (is (persistent-p instance))
      (is (equal (name-of instance) "the one")))))

(def test test/persistence/make-persistent/2 ()
  (with-transaction
    (let ((instance (make-instance 'persistence-test :name "the one")))
      (slot-makunbound instance 'persistent)
      (is (persistent-p instance)))))

(def test test/persistence/lock-class/1 ()
  (with-transaction
    (is (lock-class (find-class 'persistence-test) :wait #t)))
  (with-transaction
    (is (lock-class (find-class 'persistence-test) :wait #f))))

(def test test/persistence/lock-class/2 ()
  (with-transaction
    (lock-class (find-class 'persistence-test) :wait #t)
    (is (not
         (with-transaction
           (lock-class (find-class 'persistence-test) :wait #f))))))

(def test test/persistence/lock-instance/1 ()
  (with-one-and-two-transactions
      (make-instance 'persistence-test :name "the one")
    (is (lock-instance -instance- :wait #t)))
  (with-one-and-two-transactions
      (make-instance 'persistence-test :name "the one")
    (is (lock-instance -instance- :wait #f))))

(def test test/persistence/lock-instance/2 ()
  (let ((instance
         (with-transaction
           (make-instance 'persistence-test :name "the one"))))
    (with-transaction
      (with-reloaded-instance instance
        (lock-instance instance :wait #t))
      (is (not
           (with-transaction
             (with-reloaded-instance instance
               (lock-instance instance :wait #f))))))))

(def test test/persistence/lock-slot/1 ()
  (with-one-and-two-transactions
      (make-instance 'persistence-test :name "the one")
    (is (lock-slot -instance- 'name :wait #t)))
  (with-one-and-two-transactions
      (make-instance 'persistence-test :name "the one")
    (is (lock-slot -instance- 'name :wait #f))))

(def test test/persistence/lock-slot/2 ()
  (let ((instance
         (with-transaction
           (make-instance 'persistence-test :name "the one"))))
    (with-transaction
      (with-reloaded-instance instance
        (lock-slot instance 'name :wait #t))
      (is (not
           (with-transaction
             (with-reloaded-instance instance
               (lock-slot instance 'name :wait #f))))))))

(def persistent-class* initform-1-test ()
  ((name "Hello" :type (text 20))))

(def persistent-class* initform-2-test ()
  ((name (error "Hello") :type (text 20))))

(def test test/persistence/initform/1 ()
  (with-transaction
    (is (equal "Hello" (name-of (make-instance 'initform-1-test))))))

(def test test/persistence/initform/2 ()
  (with-transaction
    (signals error
      (make-instance 'initform-2-test))
    (values)))
