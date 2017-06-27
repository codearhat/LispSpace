;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

(def special-variable *sql-syntax-node-names* nil)

(def special-variable *sql-constructor-names* nil)

(def function import-sql-syntax-node-names (&optional (package *package*))
  (import *sql-syntax-node-names* package))

(def function import-sql-constructor-names (&optional (package *package*))
  (import *sql-constructor-names* package))

(def special-variable *sql-stream*)
(def special-variable *command-elements*)
(def special-variable *binding-variables*)
(def special-variable *binding-types*)
(def special-variable *binding-values*)

(def generic format-sql-syntax-node (node database)
  (:documentation "Formats an SQL syntax node into *sql-stream*.")

  (:method :around (node (database null))
           (error "May not call with null DATABASE"))

  (:method (node database)
           (format-sql-literal node database)))

(def function reduce-subsequences (sequence predicate reducer)
  (iter (with completely-reduced? = #t)
        (for index :from 0 :below (length sequence))
        (for reducibles = (iter (while (< index (length sequence)))
                                (for element = (elt sequence index))
                                (while (funcall predicate element))
                                (collect element)
                                (incf index)))
        (collect (if (zerop (length reducibles))
                     (progn
                       (setf completely-reduced? #f)
                       (elt sequence index))
                     (progn
                       (decf index)
                       (apply reducer reducibles)))
          :into result
          :result-type vector)
        (finally (return (values result completely-reduced?)))))

(def function vector-extend (extension vector)
  (bind ((original-length (length vector))
         (extension-length (length extension))
         (new-length (+ original-length extension-length))
         (original-dimension (array-dimension vector 0)))
    (when (< original-dimension new-length)
      (setf vector (adjust-array vector (max (* 2 original-dimension) new-length))))
    (setf (fill-pointer vector) new-length)
    (replace vector extension :start1 original-length)
    vector))

;;; from http://common-lisp.net/cgi-bin/darcsweb/darcsweb.cgi?r=plexippus-xpath-plexippus-xpath;a=annotate_shade;f=/utils.lisp

;; Cache the result of COMPILATION-BODY as long as KEYS still match.
;; This is thread-safe because the cache is replaced atomically.  We will
;; lose cache conses if threads replace them simultaneously.  But that's
;; okay, since correctness is not affected.  Losing some values is easier
;; than having to use locking, and contention is not a case we are
;; optimizing for.
;;
;; zzz extend this to use a vector of multiple cache-conses, using either
;; linear search with round-robin replacement, or using SXHASH-based
;; hashing.  Make the size of that table static, but configurable.
(def macro with-cache ((&rest keys) &body compilation-body)
  (let ((key-values '())
        (key-tests '()))
    (dolist (key keys)
      (destructuring-bind (value &key (test 'equal)) key
        (push value key-values)
        (push test key-tests)))
    (setf key-values (nreverse key-values))
    (setf key-tests (nreverse key-tests))
    (let* ((keysyms (loop repeat (length keys) collect (gensym)))
           (place (gensym))
           (previous (gensym))
           (check
            (when keysyms
              `((let ((l (cdr ,PREVIOUS)))
                  , (labels ((recurse (vars tests)
                               `(and (,(car tests) (car l) ,(car vars))
                                     ,@ (when (cdr vars)
                                          `((let ((l (cdr l)))
                                              ,(recurse (cdr vars)
                                                        (cdr tests))))))))
                      (recurse keysyms key-tests)))))))
      `(let* ((,PLACE (load-time-value (cons nil nil)))
              (,PREVIOUS (car ,PLACE))
              ,@(mapcar #'list keysyms key-values))
         (cond
           ((and ,PREVIOUS ,@check)
            (car ,PREVIOUS))
           (t
            (let ((thunk (progn ,@compilation-body)))
              (setf (car ,PLACE) (list thunk ,@keysyms))
              thunk)))))))

(def function same-backend-p (x y)
  (cl:eq (cl:type-of x) (cl:type-of y)))

;; sql statements via sql macro and custom reader compile prematurely
;; without knowing the right backend.  We need to recompile in case
;; the backend at compilation time was different from the one we are
;; actually using.
(def function expand-sql-ast-into-lambda-form-cached (syntax-node &key env database (toplevel #t))
  (declare (ignore database))
  (let ((lvars nil))
    (when env
      (hu.dwim.walker:iterate-variables-in-lexenv
       (lambda (name &key ignored? special? macro? macro-body type)
         (declare (ignore macro-body type))
         (unless (or ignored? special? macro?)
           (push name lvars)))
       env))
    `(funcall
      (with-cache ((*database* :test same-backend-p))
        (compile
         nil
         `(lambda ,',lvars
            ,(expand-sql-ast-into-lambda-form ',syntax-node
                                              :database *database*
                                              :toplevel ',toplevel))))
      ,@lvars)))

;; TODO: if sql-quote is added this should return a lambda returning
;; the syntax-node unaltered unless it is an sql-quote in which case
;; it can be process

;; FIXME this got bitrotten... the whole quasi-quoted sql
;; generation should be rebased on hu.dwim.quasi-quote, which
;; in turn should be cleaned up...
(def function expand-sql-ast-into-lambda-form (syntax-node &key database (toplevel #t))
  (bind ((*print-pretty* #f)
         (*print-circle* #f)
         (*sql-stream* (make-string-output-stream))
         (*database* (or database
                         (and (boundp '*database*)
                              *database*)
                         (make-instance 'database)))
         (*command-elements* (make-array 8 :adjustable #t :fill-pointer 0))
         (*binding-variables* (make-array 16 :adjustable #t :fill-pointer 0))
         (*binding-types* (make-array 16 :adjustable #t :fill-pointer 0))
         (*binding-values* (make-array 16 :adjustable #t :fill-pointer 0)))
    (assert *database*)
    ;; TODO (?) this formatting could be put in a load-time-value and then loading the fasl's would react to
    ;; changing *database* before loading them and use the syntax customizations specified by it.
    (format-sql-syntax-node syntax-node *database*)
    (bind ((last-command-element (get-output-stream-string *sql-stream*)))
      (unless (zerop (length last-command-element))
        (vector-push-extend last-command-element *command-elements*)))
    (flet ((constant-command-element-p (element)
             (stringp element))
           (constant-variable-p (element)
             (or (null element)
                 ;; FIXME a style-warning because SQL-UNQUOTE is defined in something we are a dependency of (see multiple instances in this file)
                 (not (typep element 'sql-unquote))))
           (constant-type-p (element)
             (not (typep element 'sql-unquote)))
           (constant-value-p (element)
             (not (typep element 'sql-unquote)))
           (process-elements (sequence vector)
             (iter (for element :in-vector sequence)
                   (collect (if (arrayp element)
                                (unless (zerop (length element))
                                  `(vector-extend ,element ,vector))
                                `(vector-push-extend ,(form-of element) ,vector)))))
           (optimize-vector (vector)
             (if (zerop (length vector))
                 #()
                 vector)))
      (bind (((:values command-elements constant-command-elements?) (reduce-subsequences *command-elements*
                                                                                         #'constant-command-element-p
                                                                                         #'string+))
             ((:values variables constant-variables?)               (reduce-subsequences *binding-variables*
                                                                                         #'constant-variable-p
                                                                                         #'vector))
             ((:values types constant-types?)                       (reduce-subsequences *binding-types*
                                                                                         #'constant-type-p
                                                                                         #'vector))
             ((:values values constant-values?)                     (reduce-subsequences *binding-values*
                                                                                         #'constant-value-p
                                                                                         #'vector))
             (expand-as-constant-command-elements? (and toplevel
                                                        constant-command-elements?))
             (expand-as-constant-variables?        (and toplevel
                                                        constant-variables?
                                                        constant-command-elements?))
             (expand-as-constant-types?            (and toplevel
                                                        constant-types?
                                                        constant-command-elements?))
             (expand-as-constant-values?           (and toplevel
                                                        constant-values?
                                                        constant-command-elements?
                                                        (every #'null *binding-variables*)))
             (body
              `(,@(unless expand-as-constant-variables?
                          (process-elements variables '*binding-variables*))
                  ,@(unless expand-as-constant-types?
                            (process-elements types '*binding-types*))
                  ,@(unless expand-as-constant-values?
                            (process-elements values '*binding-values*))
                  ,@(unless expand-as-constant-command-elements?
                            (iter (for element :in-vector command-elements)
                                  (collect (if (stringp element)
                                               `(write-string ,element *sql-stream*)
                                               element))))))
             (bindings
              (when toplevel
                `(,@(unless expand-as-constant-command-elements?
                            '((*print-pretty* #f)
                              (*print-circle* #f)
                              (*sql-stream* (make-string-output-stream))))
                    ,@(unless expand-as-constant-variables?
                              '((*binding-variables* (make-array 16 :adjustable #t :fill-pointer 0))))
                    ,@(unless expand-as-constant-types?
                              '((*binding-types* (make-array 16 :adjustable #t :fill-pointer 0))))
                    ,@(unless expand-as-constant-values?
                              '((*binding-values* (make-array 16 :adjustable #t :fill-pointer 0)))))))
             (result
              (if toplevel
                  `(values
                    ,(if expand-as-constant-command-elements? (first-elt command-elements) '(get-output-stream-string *sql-stream*))
                    ,(if expand-as-constant-variables? (optimize-vector *binding-variables*) '*binding-variables*)
                    ,(if expand-as-constant-types?     (optimize-vector *binding-types*)     '*binding-types*)
                    ,(if expand-as-constant-values?    (optimize-vector *binding-values*)    '*binding-values*))
                  '(values))))
        `(lambda ()
           ,@(when toplevel
               `((unless (typep *database* ',(class-name (class-of *database*)))
                   (error "The current value of *database* (~A) is not subtypep of the compile-time type of *database* (~S)."
                          *database* ',(class-name (class-of *database*))))))
           ,@(if bindings
                 `((bind ,bindings
                     ,@body
                     ,result))
                 `(,@body
                   ,result)))))))

(def method execute-command :around (database transaction (command function) &rest args &key bindings &allow-other-keys)
  (bind (((:values command binding-variables binding-types binding-values) (funcall command)))
    (update-binding-values binding-variables binding-types binding-values bindings)
    (remove-from-plistf args :bindings)
    (apply #'execute-command database transaction command
           :binding-types binding-types :binding-values binding-values args)))

(def function format-sql (syntax-node &key (stream t) (database *database*))
  "Formats the given SQL syntax node into the stream."
  (let* ((*print-pretty* #f)
         (*sql-stream* stream)
         (*database* database)
         (*binding-variables* (make-array 16 :adjustable #t :fill-pointer 0))
         (*binding-types* (make-array 16 :adjustable #t :fill-pointer 0))
         (*binding-values* (make-array 16 :adjustable #t :fill-pointer 0)))
    (format-sql-syntax-node syntax-node database)
    (values stream *binding-variables* *binding-types* *binding-values*)))

(def function format-sql-to-string (syntax-node &rest args &key &allow-other-keys)
  "Formats the given SQL syntax node into a string."
  (let* ((*print-pretty* #f)
         scratch
         binding-variables
         binding-types
         binding-values
         (string (with-output-to-string (stream)
                   (setf (values scratch binding-variables binding-types binding-values)
                         (apply #'format-sql syntax-node :stream stream args)))))
    (values string binding-variables binding-types binding-values)))

(def function sql-constructor-name (name)
  (format-symbol (find-package :hu.dwim.rdbms) "SQL-~A" name))

(def function sql-operator-name (name)
 (if (every (lambda (char) (char= char #\-)) (symbol-name name))
     (string-upcase name)
     (substitute #\Space #\- (string-upcase name))))

(def generic make-syntax-node (name &rest args))

(def definer syntax-node (name supers slots &rest options)
  (let ((effective-slots (delete-duplicates
                          (append (mapcan (lambda (super) (copy-list (get super :slot-names))) supers)
                                  (mapcar #'first slots)))))
    (flet ((define-format-method (method-name body)
             `(def method ,method-name ((-self- ,name) database)
                (macrolet ((format-sql-syntax-node (node)
                             `(funcall 'format-sql-syntax-node ,node database))
                           (format-sql-literal (node)
                             `(funcall 'format-sql-literal ,node database))
                           (format-sql-identifier (node)
                             `(funcall 'format-sql-identifier ,node database))
                           (format-comma-separated-list (nodes &optional format-fn)
                             `(funcall 'format-comma-separated-list ,nodes database
                                       ,@(when format-fn
                                               (list format-fn))))
                           (format-comma-separated-identifiers (nodes)
                             `(funcall 'format-comma-separated-identifiers ,nodes database))
                           (format-separated-list (nodes separator &optional format-fn)
                             `(funcall 'format-separated-list ,nodes ,separator database
                                       ,@(when format-fn
                                               (list format-fn))))
                           (format-sql-where (expression)
                             `(funcall 'format-sql-where ,expression database)))
                  (with-slots ,effective-slots -self-
                    ,@body)))))
      `(progn
         (eval-always
           (setf (get ',name :slot-names) ',effective-slots))
         (def class* ,name ,supers ,slots
           ,@(remove-if (lambda (option)
                          (starts-with-subseq "format" (string-downcase (first option))))
                        options))
         (def method print-object ((object ,name) stream)
           (print-unreadable-object (object stream :type t :identity t)
             (format stream "~s"
                     (mapcar (lambda (x) (list x
                                               (and (slot-boundp object x)
                                                    (slot-value object x))))
                             (get ',name :slot-names)))))
         (pushnew ',name *sql-syntax-node-names*)
         ,(awhen (find :format-sql-syntax-node options :key #'first)
                 (define-format-method 'format-sql-syntax-node (rest it)))
         ,(awhen (find :format-sql-identifier options :key #'first)
                 (define-format-method 'format-sql-identifier (rest it)))
         (pushnew ',name *sql-constructor-names*)
         (def method make-syntax-node ((name (eql ',name)) &rest args)
           (apply #'make-instance (cons name args)))
         (def macro ,name (&body args)
           `(make-syntax-node ',',name ,@args))
         (find-class ',name)))))

(def function format-comma-separated-list (nodes database &optional (format-fn 'format-sql-syntax-node))
  (format-separated-list nodes #\, database format-fn))

(def function format-comma-separated-identifiers (nodes database)
  (format-comma-separated-list nodes database 'format-sql-identifier))

(def function format-separated-list (nodes separator database &optional (format-fn 'format-sql-syntax-node))
  (if (typep nodes 'sql-unquote)
      (progn
        (assert (not (spliced-p nodes)))
        (push-form-into-command-elements
         `(format-separated-list ,(form-of nodes) ,separator *database* ',format-fn)))
      (iter (for node :in-sequence nodes)
            (unless (first-iteration-p)
              (unless (eq #\, separator)
                (write-char #\Space *sql-stream*))
              (if (typep separator 'character)
                  (write-char separator *sql-stream*)
                  (write-string separator *sql-stream*))
              (write-char #\Space *sql-stream*))
            (if (typep node 'sql-unquote)
                (if (spliced-p node)
                    (push-form-into-command-elements
                     `(format-separated-list ,(form-of node) ,separator *database* ',format-fn))
                    (expand-sql-unquote node database format-fn))
                (funcall format-fn node database)))))

(def macro format-char (character)
  `(write-char
    ,(if (typep character 'string)
         (progn
           (assert (= (length character) 1) nil "format-char must be called with a character or a 1 character long string")
           (elt character 0))
         character) *sql-stream*))

(def macro format-string (string)
  `(write-string ,string *sql-stream*))

(def function print-number-to-sql-string (number)
  (etypecase number
    (integer (princ-to-string number))
    (ratio (format nil "~F" (coerce number 'double-float)))
    (float (format nil "~F" number))))

(def macro format-number (number)
  `(write-string (print-number-to-sql-string ,number) *sql-stream*))

(def function format-sql-where (where database)
  (when where
    (format-string " WHERE ")
    (format-sql-syntax-node where database)))
