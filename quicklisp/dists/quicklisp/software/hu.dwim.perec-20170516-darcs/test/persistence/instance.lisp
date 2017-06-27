;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/persistence/instance :in test/persistence))

(def persistent-class* instance-test ()
  ((code :type (or null integer-16))))

(def test test/persistence/instance/created/1 ()
  (with-transaction
    (let ((instance (make-instance 'instance-test)))
      (is (created-p instance))
      (is (not (modified-p instance)))
      (is (not (deleted-p instance))))))

(def test test/persistence/instance/created/2 ()
  (with-transaction
    (let ((instance (make-instance 'instance-test)))
      (purge-instance instance)
      (is (not (created-p instance)))
      (is (not (modified-p instance)))
      (is (not (deleted-p instance))))))

(def test test/persistence/instance/created/3 ()
  (with-transaction
    (let ((instance (make-instance 'instance-test)))
      (make-transient instance)
      (is (not (created-p instance)))
      (is (not (modified-p instance)))
      (is (not (deleted-p instance))))))

(def test test/persistence/instance/not-modified/1 ()
  (let ((instance
         (with-transaction
           (make-instance 'instance-test))))
    (with-transaction
      (revive-instance instance)
      (is (not (created-p instance)))
      (is (not (modified-p instance)))
      (is (not (deleted-p instance))))))

(def test test/persistence/instance/modified/1 ()
  (let ((instance
         (with-transaction
           (make-instance 'instance-test))))
    (with-transaction
      (revive-instance instance)
      (setf (code-of instance) 1)
      (is (not (created-p instance)))
      (is (modified-p instance))
      (is (not (deleted-p instance))))))

(def test test/persistence/instance/modified/2 ()
  (let ((instance
         (with-transaction
           (make-instance 'instance-test))))
    (with-transaction
      (revive-instance instance)
      (setf (code-of instance) 1)
      (purge-instance instance)
      (is (not (created-p instance)))
      (is (not (modified-p instance)))
      (is (deleted-p instance)))))

(def test test/persistence/instance/deleted/1 ()
  (let ((instance
         (with-transaction
           (make-instance 'instance-test))))
    (with-transaction
      (revive-instance instance)
      (purge-instance instance)
      (is (not (created-p instance)))
      (is (not (modified-p instance)))
      (is (deleted-p instance)))))
