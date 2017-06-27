;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/query/select/order-by :in test/query/select))

(def function check-ordered (classes order-spec)
  (bind ((compare-fn (generate-compare-fn order-spec))
;; PORT: is does not support macros inside, yet to come
         (pass (iter (for class in classes)
                     (for prev previous class)
                     (always (or (null prev)
                                 (funcall compare-fn prev class))))))
;; PORT:
;;"Ordering of ~a by ~a failed." classes order-spec)))
    (is pass)))

(def function generate-compare-fn (order-spec)
  (lambda (class1 class2)
    (iter (for (dir attr) on order-spec by 'cddr)
          (for attr-value-1 = (slot-value class1 attr))
          (for attr-value-2 = (slot-value class2 attr))
          (for compare-fn = (ecase dir
                              (:ascending 'less-or-equal-p)
                              (:descending 'greater-or-equal-p)))
          (cond
            ((funcall compare-fn attr-value-1 attr-value-2) (return #t))
            ((not (equal attr-value-1 attr-value-2)) (return #f)))
          (finally (return #t)))))

(def macro run-order-by-test (&body body)
  `(with-fixture fill-data-6
     (run-queries
       ,@body)))

(def persistent-class* order-by-test ()
  ((int-attr :type integer-32)
   (str-attr :type (text 10))
   (date-attr :type date)))

(def fixture fill-data-6
  (with-transaction
    (purge-instances 'order-by-test)
    (bind ((count 10)
           (int-values (iter (for i from 0 below count) (collect i)))
           (str-values (iter (for i from 0 below count) (collect (string (digit-char i)))))
           (date-values (iter (for i from 0 below count) (collect (make-timestamp :day i)))))
      (macrolet ((random-element (list)
                   (with-unique-names (element)
                     `(let ((,element (nth (random (length ,list)) ,list)))
                       (setf ,list (remove ,element ,list))
                       ,element))))
        (iter (for i from 0 below count)
              (make-instance 'order-by-test
                             :int-attr (random-element int-values)
                             :str-attr (random-element str-values)
                             :date-attr (random-element date-values))))))
  (-body-))

(def test test/query/select/order-by/integer/asc ()
  (run-order-by-test
    (check-ordered
     (select (o)
       (from (o order-by-test))
       (order-by :ascending (int-attr-of o)))
     (list :ascending 'int-attr))))

(def test test/query/select/order-by/integer/desc ()
  (run-order-by-test
    (check-ordered
     (select (o)
       (from (o order-by-test))
       (order-by :descending (int-attr-of o)))
     (list :descending 'int-attr))))

(def test test/query/select/order-by/string/asc ()
  (run-order-by-test
    (check-ordered
     (select (o)
       (from (o order-by-test))
       (order-by :ascending (str-attr-of o)))
     (list :ascending 'str-attr))))

(def test test/query/select/order-by/string/desc ()
  (run-order-by-test
    (check-ordered
     (select (o)
       (from (o order-by-test))
       (order-by :descending (str-attr-of o)))
     (list :descending 'str-attr))))

(def test test/query/select/order-by/date/asc ()
  (run-order-by-test
    (check-ordered
     (select (o)
       (from (o order-by-test))
       (order-by :ascending (date-attr-of o)))
     (list :ascending 'date-attr))))

(def test test/query/select/order-by/date/desc ()
  (run-order-by-test
    (check-ordered
     (select (o)
       (from (o order-by-test))
       (order-by :descending (date-attr-of o)))
     (list :descending 'date-attr))))

(def test test/query/select/order-by/all ()
  (run-order-by-test
    (check-ordered
     (select (o)
       (from (o order-by-test))
       (order-by :ascending (int-attr-of o) :descending (str-attr-of o)))
     (list :ascending 'int-attr :descending 'str-attr))))

(def test test/query/select/order-by/expression ()
  (run-order-by-test
    (check-ordered
     (select (o)
       (from (o order-by-test))
       (order-by :ascending (- (int-attr-of o))))
     (list :descending 'int-attr))))

(def test test/query/select/order-by/error ()
  (run-order-by-test
    (signals error
      (select (o)
        (from (o order-by-test))
        (order-by :ascending (int-attr-of 'o))))))
