;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;
;;; Set interface
;;;
(defmacro apply-key* (key element)
  `(if ,key
       (funcall ,key ,element)
       ,element))

(def function set-as-hashtable (set &key key (test 'eql))
  (if (listp set)
      (iter (with count = (length set))
            (with result = (make-hash-table :size count :test test))
            (for element :in set)
            (setf (gethash (apply-key* key element) result) element)
            (finally (return result)))
      set))

(def function set-as-list (set)
  (if (hash-table-p set)
      (iter (for (key value) :in-hashtable set)
            (collect value))
      set))

(def function hashtable-subset (set-1 set-2)
  (iter (for (key value) :in-hashtable set-1)
        (unless (gethash key set-2)
          (return #f))
        (finally (return #t))))

(def function hashtable-intersection (set-1 set-2)
  (iter (with result = (make-hash-table))
        (for (key value) :in-hashtable set-1)
        (when (gethash key set-2)
          (setf (gethash key result) value))
        (finally (return result))))

(def function hashtable-union (set-1 set-2)
  (prog1-bind result (make-hash-table)
    (iter (for (key value) :in-hashtable set-1)
          (setf (gethash key result) value))
    (iter (for (key value) :in-hashtable set-2)
          (setf (gethash key result) value))))

(def function hashtable-difference (set-1 set-2)
  (iter (with result = (make-hash-table))
        (for (key value) :in-hashtable set-1)
        (unless (gethash key set-2)
          (setf (gethash key result) value))
        (finally (return (unless (zerop (hash-table-count result))
                           result)))))



(def constant +list-based-intersection-limit+ 80)
(def constant +list-based-difference-limit+ 80)
(def constant +list-based-set-exclusive-or-limit+ 80)
(def constant +list-based-subsetp-limit+ 80)

(def (function io) union* (list1 list2 &rest args &key key (test #'eql testp) (test-not nil notp))
  (declare (ignore key test testp test-not notp))
  (apply #'union list1 list2 args))

(def (function o) intersection* (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  "Return the union of LIST1 and LIST2."
  (declare (type list list1 list2)
           (inline member))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))

  (let ((n1 (length list1))
        (n2 (length list2))
        (key (and key (ensure-function key)))
        (test (if notp
                  (let ((test-not-fun (ensure-function test-not)))
                    (lambda (x y) (not (funcall test-not-fun x y))))
                  (ensure-function test))))
    (multiple-value-bind (short long n-short)
        (if (< n1 n2)
            (values list1 list2 n1)
            (values list2 list1 n2))
      (if (or (< n-short +list-based-intersection-limit+)
              (not (member test (list #'eq #'eql #'equal #'equalp))))
          (let ((res nil))
            (dolist (elt long)
              (if (member (apply-key* key elt) short :key key :test test)
                  (push elt res)))
            res)
          (let ((table (make-hash-table :test test :size n-short))
                (intersection nil))
            (dolist (elt short)
              (setf (gethash (apply-key* key elt) table) elt))
            (dolist (elt long)
              (multiple-value-bind (value present-p) (gethash (apply-key* key elt) table)
                (declare (ignore value))
                (when present-p
                  (push elt intersection))))
            intersection)))))

(def (function o) set-difference* (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  "Return the difference of LIST1 and LIST2."
  (declare (type list list1 list2)
           (inline member))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))

  (let ((n2 (length list2))
        (key (and key (ensure-function key)))
        (test (if notp
                  (let ((test-not-fun (ensure-function test-not)))
                    (lambda (x y) (not (funcall test-not-fun x y))))
                  (ensure-function test))))
    (if (or (< n2 +list-based-difference-limit+)
            (not (member test (list #'eq #'eql #'equal #'equalp))))

        (if (null list2)
            list1
            (let ((res nil))
              (dolist (elt list1)
                (if (not (member (apply-key* key elt) list2 :key key :test test))
                    (push elt res)))
              res))
          
        (let ((table (make-hash-table :test test :size n2))
              (difference nil))
          (dolist (elt list2)
            (setf (gethash (apply-key* key elt) table) elt))
          (dolist (elt list1)
            (multiple-value-bind (value present-p) (gethash (apply-key* key elt) table)
              (declare (ignore value))
              (unless present-p
                (push elt difference))))
          difference))))

(def (function o) set-exclusive-or* (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  "Return the union of LIST1 and LIST2."
  (declare (type list list1 list2)
           (inline member))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))

  (let ((n1 (length list1))
        (n2 (length list2))
        (key (and key (ensure-function key)))
        (test (if notp
                  (let ((test-not-fun (ensure-function test-not)))
                    (lambda (x y) (not (funcall test-not-fun x y))))
                  (ensure-function test))))
    (declare (type function test))
    (prog1-bind result nil
      (cond ((or (and (< n1 +list-based-set-exclusive-or-limit+)
                      (< n2 +list-based-set-exclusive-or-limit+))
                 (not (member test (list #'eq #'eql #'equal #'equalp))))
          
             (dolist (elt list1)
               (unless (member (apply-key* key elt) list2 :test test)
                 (push elt result)))
             (let ((test (if (or testp notp) (lambda (x y) (funcall test y x)) test)))
               (dolist (elt list2)
                 (unless (member (apply-key* key elt) list1 :test test)
                   (push elt result)))))
          
            ((and (>= n1 +list-based-set-exclusive-or-limit+)
                  (>= n2 +list-based-set-exclusive-or-limit+))
             (let ((table1 (make-hash-table :test test :size n1))
                   (table2 (make-hash-table :test test :size n2)))
               (dolist (elt list1)
                 (setf (gethash (apply-key* key elt) table1) elt))
               (dolist (elt list2)
                 (setf (gethash (apply-key* key elt) table2) elt))
               (dolist (elt list1)
                 (multiple-value-bind (value present-p) (gethash (apply-key* key elt) table2)
                   (declare (ignore value))
                   (unless present-p
                     (push elt result))))
               (dolist (elt list2)
                 (multiple-value-bind (value present-p) (gethash (apply-key* key elt) table1)
                   (declare (ignore value))
                   (unless present-p
                     (push elt result))))))

            ((>= n1 +list-based-set-exclusive-or-limit+)
             (let ((table1 (make-hash-table :test test :size n1)))
               (dolist (elt list1)
                 (setf (gethash (apply-key* key elt) table1) elt)
                 (unless (member (apply-key* key elt) list2 :test test)
                   (push elt result)))
               (dolist (elt list2)
                 (multiple-value-bind (value present-p) (gethash (apply-key* key elt) table1)
                   (declare (ignore value))
                   (unless present-p
                     (push elt result))))))

            (t
             (let ((table2 (make-hash-table :test test :size n2)))
               (dolist (elt list2)
                 (setf (gethash (apply-key* key elt) table2) elt))
               (dolist (elt list1)
                 (multiple-value-bind (value present-p) (gethash (apply-key* key elt) table2)
                   (declare (ignore value))
                   (unless present-p
                     (push elt result))))
               (dolist (elt list2)
                 (unless (member (apply-key* key elt) list1 :test test)
                   (push elt result)))))))))

(def (function o) subsetp* (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  "Return T if every element in LIST1 is also in LIST2."
  (declare (type list list1 list2)
           (inline member))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))

  (let ((n2 (length list2))
        (key (and key (ensure-function key)))
        (test (if notp
                  (let ((test-not-fun (ensure-function test-not)))
                    (lambda (x y) (not (funcall test-not-fun x y))))
                  (ensure-function test))))
    (if (or (null list1) (length= 1 list1)
            (< n2 +list-based-subsetp-limit+)
            (not (member test (list #'eq #'eql #'equal #'equalp))))

        (progn
          (dolist (elt list1)
            (unless (member (apply-key* key elt) list2 :key key :test test)
              (return-from subsetp* #f)))
          #t)

        (let ((table (make-hash-table :test test :size n2)))
          (dolist (elt list2)
            (setf (gethash (apply-key* key elt) table) elt))
          (dolist (elt list1)
            (multiple-value-bind (value present-p) (gethash (apply-key* key elt) table)
              (declare (ignore value))
              (unless present-p
                (return-from subsetp* #f))))
          #t))))


(def function sorted-list-union (list1 list2 compare)
  (iter (cond
          ((and list1 list2)
           (ecase (funcall compare (car list1) (car list2))
             (= (collect (car list1))
                (setf list1 (cdr list1)
                      list2 (cdr list2)))
             (< (collect (car list1)) (setf list1 (cdr list1)))
             (> (collect (car list2)) (setf list2 (cdr list2)))))
          (list1
           (collect (car list1))  (setf list1 (cdr list1)))
          (list2
           (collect (car list2)) (setf list2 (cdr list2)))
          (t
           (finish)))))

(def function sorted-list-intersection (list1 list2 compare)
  (iter (while (and list1 list2))
        (ecase (funcall compare (car list1) (car list2))
          (= (collect (car list1))
             (setf list1 (cdr list1)
                   list2 (cdr list2)))
          (< (setf list1 (cdr list1)))
          (> (setf list2 (cdr list2))))))

(def function sorted-list-difference (list1 list2 compare)
  (append
   (iter :outer
         (for element2 :in list2)
         (iter (for element1 :in list1)
               (ecase (funcall compare element1 element2)
                 (< (in :outer (collect element1) (setf list1 (cdr list1))))
                 (= (setf list1 (cdr list1)) (leave))
                 (> (leave)))))
   list1))

(def function sorted-list-subsetp (list1 list2 compare)
  (iter (for element1 :in list1)
        (always (iter (while list2)
                      (thereis (ecase (funcall compare element1 (car list2))
                                 (< (leave #f))
                                 (= (setf list2 (cdr list2)) #t)
                                 (> (setf list2 (cdr list2)) #f)))))))

(def function sorted-list-equal (list1 list2 compare)
  (iter (for cell1 :on list1)
        (for cell2 :on list2)
        (unless (eq '= (funcall compare (car cell1) (car cell2)))
          (return-from sorted-list-equal #f))
        (when (or (endp (cdr cell1)) (endp (cdr cell2)))
          (return-from sorted-list-equal (and (endp (cdr cell1)) (endp (cdr cell2)))))
        (finally (return #t))))

#+nil
(progn
  (defvar set-1 (iter (for i :from 1 :to 1000) (collect i)))
  (defvar set-2 (iter (for i :from 1 :to 1000) (when (zerop (mod i 2)) (collect i))))
  (defvar set-3 (iter (for i :from 1 :to 1000) (when (zerop (mod i 3)) (collect i))))
  (cl:time (iter (repeat 1000) (intersection set-2 set-3)))
  (cl:time (iter (repeat 1000) (intersection* set-2 set-3)))
  (cl:time (iter (repeat 1000) (set-difference set-2 set-3)))
  (cl:time (iter (repeat 1000) (set-difference* set-2 set-3)))
  (cl:time (iter (repeat 1000) (set-exclusive-or set-2 set-3)))
  (cl:time (iter (repeat 1000) (set-exclusive-or* set-2 set-3)))
  (cl:time (iter (repeat 1000) (subsetp set-2 set-1)))
  (cl:time (iter (repeat 1000) (subsetp* set-2 set-1))))

