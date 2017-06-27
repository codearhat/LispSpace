;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def (constant e) +top-type+ t)

(def (constant e) +bottom-type+ nil)

;; TODO promote these to alexandria?
(def (type e) function-name ()
  '(and (or symbol
            (cons (eql setf)
                  (cons (and symbol (not (member nil t)))
                        null)))
        (not (member null t))))

(def (type e) function-designator ()
  '(or function function-name))

#||

if we strictly followed CLHS, then it should be the following:

(def (type e) function-designator ()
  '(or function '(and symbol (not (member nil t)))))

(def (type e) extended-function-designator ()
  '(or function function-name))

||#

(def (function e) expand-type (type &optional env)
  (declare (ignorable type env))
  #*((:sbcl (sb-ext:typexpand type env))
     (t #.(not-yet-implemented/crucial-api 'expand-type))))

(def (function e) expand-type-1 (type &optional env)
  (declare (ignorable type env))
  #*((:sbcl (sb-ext:typexpand-1 type env))
     (t #.(not-yet-implemented/crucial-api 'expand-type))))

(def (function e) type-instance-count-upper-bound (input-type)
  ;; FIXME this is a very good example where a docstring or comment would have helped...
  (flet ((body (type)
           (etypecase type
             (symbol
              (case type
                ((nil) 0)
                (null 1)
                (base-char 128)
                (boolean 2)
                (single-float (expt 2 32))
                ;; TODO maybe just (1+ most-positive-fixnum) ?
                (fixnum #.(expt 2 (integer-length most-positive-fixnum)))))
             (cons
              (case (first type)
                (eql 1)
                (member
                 (1- (length type)))
                (integer
                 (when (length= type 3)
                   (bind ((lower-bound (second type))
                          (upper-bound (third type)))
                     (1+ (- (if (consp upper-bound)
                                (1- (first upper-bound))
                                upper-bound)
                            (if (consp lower-bound)
                                (1+ (first lower-bound))
                                lower-bound))))))
                ((signed-byte unsigned-byte)
                 (expt 2 (second type)))
                (simple-base-string
                 (expt 128 (second type)))
                (not nil)
                (or
                 (iter (for element :in (cdr type))
                       (aif (type-instance-count-upper-bound element)
                            (summing it)
                            (return nil))))
                (and
                 (iter (for element :in (cdr type))
                       (awhen (type-instance-count-upper-bound element)
                         (minimizing it)))))))))
    (or (body input-type)
        (body (expand-type input-type)))))

;; TODO: sort the result with some natural sort
(def (function e) type-instance-list (type)
  (setf type (expand-type type))
  (etypecase type
    (symbol
     (case type
       ((nil) nil)
       (null '(nil))))
    (cons
     (case (first type)
       (eql
        (second type))
       (member
        (cdr type))
       (integer
        (iter (with lower-bound = (second type))
              (with upper-bound = (third type))
              (for i
                   :from (if (consp lower-bound)
                             (1+ (first lower-bound))
                             lower-bound)
                   :to (if (consp upper-bound)
                           (1- (first upper-bound))
                           upper-bound))
              (collect i)))
       (unsigned-byte
        (iter (for i :from 0 :below (expt 2 (second type)))
              (collect i)))
       (signed-byte
        (iter (with limit = (expt 2 (1- (second type))))
              (for i :from (- limit) :below limit)
              (collect i)))
       (not nil)
       (or
        (reduce 'union (cdr type) :key 'type-instance-list))
       (and
        (reduce 'intersection (cddr type) :key 'type-instance-list :initial-value (type-instance-list (second type))))))))

#+(:or) ;; half baked...
(def (function e) a-valid-value-for-type (input-type &key (otherwise nil))
  (flet ((body (type)
           (etypecase type
             (symbol
              (ecase type
                ((nil) (handle-otherwise/value otherwise))
                (null nil)
                (base-char #\Space)
                (boolean 't)
                (single-float 1.0s0)
                ((fixnum integer) 0)
                (ratio 1/3)
                (number 0)))
             (cons
              (case (first type)
                (eql (second type))
                (member (second type))
                (integer (second type))
                ((signed-byte unsigned-byte) 0)
                (simple-base-string "")
                (not (handle-otherwise/value otherwise))
                #+nil
                (or (iter (for element :in (cdr type))
                          (aif (type-instance-count-upper-bound element)
                               (summing it)
                               (return nil))))
                #+nil
                (and
                 (iter (for element :in (cdr type))
                       (awhen (type-instance-count-upper-bound element)
                         (minimizing it)))))))))
    (or (body input-type)
        (body (expand-type input-type)))))
