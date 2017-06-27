;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;; THE CONTENT OF THIS FILE IS COPIED OVER FROM SOME OTHER LIBRARIES TO DECREASE THE NUMBER OF DEPENDENCIES

(def (function o) symbol->canonical-name (symbol)
  "Returns the package name and symbol name concatenated."
  (declare (type symbol symbol))
  ;; TODO: check for valid symbol names and ':'
  (bind ((package (symbol-package symbol)))
    (if package
        (string+ (package-name package) "::" (symbol-name symbol))
        (string+  "#:" (symbol-name symbol)))))

(def (function o) canonical-name->symbol (name)
  (declare (type string name))
  (bind ((position (position #\: name))
         (package-name (subseq name 0 position))
         (symbol-name (subseq name (or (position #\: name :start (1+ position) :test-not #'char=)
                                       (1+ position)))))
    (if (string= package-name "#")
        (make-symbol symbol-name)
        (bind ((package (find-package package-name)))
          (unless package
            (error "~S: failed to find package while trying to convert ~S" 'canonical-name->symbol name))
          (intern symbol-name package)))))

(def function concatenate-symbol (&rest args)
  "Args are processed as parts of the result symbol with an exception: when a package is encountered then it is stored as the target package at intern."
  (let* ((package nil)
         (symbol-name (string-upcase
                       (with-output-to-string (str)
                         (dolist (arg args)
                           (typecase arg
                             (string (write-string arg str))
                             (package (setf package arg))
                             (symbol (unless package
                                       (setf package (symbol-package arg)))
                                     (write-string (symbol-name arg) str))
                             (integer (write-string (princ-to-string arg) str))
                             (character (write-char arg) str)
                             (t (error "Cannot convert argument ~S to symbol" arg))))))))
    (if package
        (intern symbol-name package)
        (intern symbol-name))))

(def function hasf (plist indicator)
  (not (eq (getf plist indicator :unbound) :unbound)))

(def function rcons (car cdr cons)
  "Returns a cons having CAR as car and CDR as cdr reusing CONS if possible."
  (if (and (eq car (car cons)) (eq cdr (cdr cons)))
      cons
      (cons car cdr)))

(def function find-tree-root (node parent-function)
  (find-on-parent-chain node parent-function
                        (lambda (node)
                          (if (funcall parent-function node)
                              nil
                              node))))

(def function find-on-parent-chain (node parent-function map-function)
  (iter (for current-node :initially node :then (funcall parent-function current-node))
        (while current-node)
        (awhen (funcall map-function current-node)
          (return it))))

(def macro bind-cartesian-product (((&rest variables) lst) &body body)
  (labels ((generate (variables l)
             (if (cdr variables)
                 `(dolist (,(car variables) ,l)
                    ,(generate (cdr variables) l))
                 `(dolist (,(car variables) ,l)
                    ,@body))))
    (if variables
        (with-unique-names (l)
          `(let ((,l ,lst))
             ,(generate variables l)))
        nil)))

(def macro bind-cartesian-product* (names-values-pairs &body forms)
  (if names-values-pairs
      (bind ((names-and-values (first names-values-pairs))
             (names (first names-and-values))
             (values (rest names-and-values)))
        (cons 'progn
              (iter (for value :in values)
                    (collect `(bind ((,names ,value))
                                (bind-cartesian-product* ,(rest names-values-pairs)
                                  ,@forms))))))
      `(progn
         ,@forms)) )

(def macro dopairs ((var1 var2 list) &body body)
  (with-unique-names (cell)
    `(iter (for ,cell :on ,list)
           (for ,var1 = (car ,cell))
           (iter (for ,var2 :in (cdr ,cell))
                 ,@body))))

(def function lessp (obj1 obj2)
  (typecase obj1
    (real (< obj1 obj2))
    (string (string< obj1 obj2))
    (character (char< obj1 obj2))
    (timestamp (timestamp< obj1 obj2))))

(def function less-or-equal-p (obj1 obj2)
  (typecase obj1
    (real (<= obj1 obj2))
    (string (string<= obj1 obj2))
    (character (char<= obj1 obj2))
    (timestamp (timestamp<= obj1 obj2))))

(def function greaterp (obj1 obj2)
  (typecase obj1
    (real (> obj1 obj2))
    (string (string> obj1 obj2))
    (character (char> obj1 obj2))
    (timestamp (timestamp> obj1 obj2))))

(def function greater-or-equal-p (obj1 obj2)
  (typecase obj1
    (real (>= obj1 obj2))
    (string (string>= obj1 obj2))
    (character (char>= obj1 obj2))
    (timestamp (timestamp>= obj1 obj2))))

(def function combine-with (op list-or-item item)
  (cond
    ((null list-or-item) item)
    ((and (listp list-or-item) (eq (car list-or-item) op))
     (append list-or-item (list item)))
    (t (list op list-or-item item))))

(def (function io) generalized-boolean->boolean (value)
  (if value #t #f))

(def function permute (vector indices)
  (let ((vector-copy (make-array (length vector))))
    (declare (dynamic-extent vector-copy))
    (iter (for i :from 0 :below (length vector))
          (setf (aref vector-copy i)
                (aref vector (aref indices i))))
    (replace vector vector-copy)))

;; TODO get rid of this, used only by obsolete defptype
(def macro with-lambda-parsing ((lambda-form &key finally) &body body)
  (with-unique-names (cell)
    `(iter
       (with -in-keywords- = #f)
       (with -in-optionals- = #f)
       (with -rest-variable-name- = nil)
       (for ,cell :first ,lambda-form :then (cdr ,cell))
       (while ,cell)
       (for -variable-name- = (if (or -in-optionals-
                                      -in-keywords-)
                                  (first (ensure-list (car ,cell)))
                                  (car ,cell)))
       (for -default-value- = (if (or -in-optionals-
                                      -in-keywords-)
                                  (second (ensure-list (car ,cell)))
                                  (car ,cell)))
       (case -variable-name-
         (&optional (setf -in-optionals- #t))
         (&key (setf -in-keywords- #t)
               (setf -in-optionals- #f))
         (&allow-other-keys)
         (&rest (setf -rest-variable-name- (car (cdr ,cell)))
                (setf ,cell (cdr ,cell)))
         (t ,@body))
       (finally ,@finally))))

(def function lambda-list-to-funcall-list (args)
  (with-lambda-parsing (args :finally ((return (values result -rest-variable-name-))))
    (if -in-keywords-
        (progn
          (collect (intern (symbol-name (first (ensure-list -variable-name-)))
                           #.(find-package "KEYWORD")) :into result)
          (collect -variable-name- :into result))
        (collect -variable-name- :into result))))

(def function lambda-list-to-funcall-expression (function args)
  (bind (((:values arg-list rest-variable) (lambda-list-to-funcall-list args)))
    (if rest-variable
        `(apply ,function ,@arg-list ,rest-variable)
        `(funcall ,function ,@arg-list))))

(def function lambda-list-to-variable-list (args &key (include-defaults #f) (include-&rest #f))
  (with-lambda-parsing (args :finally ((return (if (and include-&rest
                                                   -rest-variable-name-)
                                              (cons -rest-variable-name- result)
                                              result))))
    (collect (if include-defaults
                 (list -variable-name- -default-value-)
                 -variable-name-)
      :into result)))

(defun binary-search (value vector &key (key #'identity) (sort-fn #'<))
  (declare (vector vector))
  (labels ((recurse (start end)
             (if (< start end)
                 (let* ((i (+ start (truncate (- end start) 2)))
                        (elt (aref vector i))
                        (key-value (funcall key elt)))
                   (cond ((funcall sort-fn value key-value)
                          (recurse start i))
                         ((funcall sort-fn key-value value)
                          (recurse (1+ i) end))
                         (t
                          i)))
                 (- (1+ start)))))
    (recurse 0 (length vector))))

(defun lower-bound (value vector &key (key #'identity) (sort-fn #'<))
  "Returns the lowest index where the VALUE can be inserted into the sorted VECTOR without messing up the ordring."
  (declare (vector vector))
  (bind ((index (binary-search value vector :key key :sort-fn sort-fn)))
    (if (< index 0)
        (1- (- index))
        (iter (for i from (1- index) downto 0)
              (when (funcall sort-fn (funcall key (aref vector i)) value)
                (return (1+ i)))
              (finally (return 0))))))

(defun upper-bound (value vector &key (key #'identity) (sort-fn #'<))
  "Returns the greatest index where the VALUE can be inserted into the sorted VECTOR without messing up the ordering."
  (declare (vector vector))
  (bind ((index (binary-search value vector :key key :sort-fn sort-fn))
         (size (length vector)))
    (if (< index 0)
        (1- (- index))
        (iter (for i from (1+ index) below size)
              (when (funcall sort-fn value (funcall key (aref vector i)))
                (return i))
              (finally (return size))))))

(defun negate-boolean-form (form)
  (if (and (consp form) (eq (first form) 'not))
      (second form)
      `(not ,form)))

(defmacro implies (premise conclusion)
  `(or ,(negate-boolean-form premise)
       ,conclusion))

(defun but-last-elt (sequence &optional (n 1))
  (subseq sequence 0 (- (length sequence) n)))

(defun every* (predicate first-sequence &rest other-sequences)
  (bind ((first-length (length first-sequence)))
    (and (every [length= first-length !1] other-sequences)
         (apply #'every predicate first-sequence other-sequences))))

(defmacro swap (place-1 place-2)
  `(rotatef ,place-1 ,place-2))

;; TODO this should come from a lib...
(def generic generic-equal (left right)
  (:method (left right)
    (equal left right))
  (:method ((left timestamp) (right timestamp))
    (timestamp= left right))
  (:method ((left sequence) (right sequence))
    (every* #'generic-equal left right)))
