;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; Syntax

(defun make-exp (op &rest args)
  (cons op args))

(defun op (sentence)
  (if (atomic-clause? sentence)
      sentence
      (car sentence)))

(defun args (sentence)
  (cdr sentence))

(defun arg1 (sentence)
  (second sentence))

(defun atomic-clause? (sentence)
  "An atomic clause has no connectives or quantifiers."
  (or (atom sentence)
      (not (member (car sentence) '(and or not)))))

(defun literal-clause? (sentence)
  "A literal is an atomic clause or a negated atomic clause."
  (or (atomic-clause? sentence)
      (and (negative-clause? sentence) (atomic-clause? (arg1 sentence)))))

(defun negative-clause? (sentence)
  "A negative clause has NOT as the operator."
  (eq (car sentence) 'not))

;;;;;;
;;; Conjunctive normal form

(defun ->cnf (p)
  "Convert a sentence p to conjunctive normal form [p 279-280]."
  ;; That is, return (and (or ...) ...) where 
  ;; each of the conjuncts has all literal disjuncts.
  (case (op p)
    ((not) (let ((p2 (move-not-inwards (arg1 p))))
             (if (literal-clause? p2) p2 (->cnf p2))))
    ((and) (conjunction (mappend [conjuncts (->cnf !1)] (args p))))
    ((or)  (merge-disjuncts (mapcar '->cnf (args p))))
    (otherwise p)))

(defun ->dnf (p)
  "Convert a sentence p to disjunctive normal form [p 279-280]."
  ;; That is, return (and (or ...) ...) where 
  ;; each of the disjuncts has all literal conjuncts.
  (case (op p)
    ((not) (let ((p2 (move-not-inwards (arg1 p))))
             (if (literal-clause? p2) p2 (->dnf p2))))
    ((or)  (disjunction (mappend [disjuncts (->dnf !1)] (args p))))
    ((and) (merge-conjuncts (mapcar '->dnf (args p))))
    (otherwise p)))

(defun move-not-inwards (p)
  "Given P, return ~P, but with the negation moved as far in as possible."
  (case (op p)
    ((#t) #f)
    ((#f) #t)
    ((not) (arg1 p))
    ((and) (disjunction (mapcar #'move-not-inwards (args p))))
    ((or)  (conjunction (mapcar #'move-not-inwards (args p))))
    (otherwise (make-exp 'not p))))

(defun merge-disjuncts (disjuncts)
  "Return a CNF expression for the disjunction."
  ;; The argument is a list of disjuncts, each in CNF.
  (case (length disjuncts)
    (0 #f)
    (1 (first disjuncts))
    (t (conjunction
        (iter outer (for y in (conjuncts (merge-disjuncts (rest disjuncts))))
              (iter (for x in (conjuncts (first disjuncts)))
                    (in outer (collect (disjunction (append (disjuncts x) (disjuncts y)))))))))))

(defun merge-conjuncts (conjuncts)
  "Return a DNF expression for the conjunction."
  ;; The argument is a list of disjuncts, each in CNF.
  (case (length conjuncts)
    (0 #t)
    (1 (first conjuncts))
    (t (disjunction
        (iter outer (for y in (disjuncts (merge-conjuncts (rest conjuncts))))
              (iter (for x in (disjuncts (first conjuncts)))
                    (in outer (collect (conjunction (append (conjuncts x) (conjuncts y)))))))))))

;;;;;;
;;; Helpers

(defun conjuncts (sentence)
  "Return a list of the conjuncts in this sentence."
  (cond ((eq (op sentence) 'and) (args sentence))
        ((eq sentence #t) nil)
        (t (list sentence))))

(defun disjuncts (sentence)
  "Return a list of the disjuncts in this sentence."
  (cond ((eq (op sentence) 'or) (args sentence))
        ((eq sentence #f) nil)
        (t (list sentence))))

(defun conjunction (args)
  "Form a disjunction with these args."
  (case (length args)
    (0 #t)
    (1 (first args))
    (otherwise `(and ,@args))))

(defun disjunction (args)
  "Form a disjunction with these args."
  (case (length args)
    (0 #f)
    (1 (first args))
    (otherwise `(or ,@args))))

(defun simplify-boolean-form (form)
  "Makes the following simplifications on form:
   (not false)                -> true
   (not true)                 -> false
   (not (not x))              -> x
   (or)                       -> false
   (or x)                     -> x
   (or x... false y...)       -> (or x... y...)
   (or x... true y...)        -> true
   (or x... (or y...) z...)   -> (or x... y... z...)
   (and)                      -> true
   (and x)                    -> x
   (and x... true y...)       -> (and x... y...)
   (and x... false y...)      -> false
   (and x... (and y...) z...) -> (and x... y... z...)

where x, y and z are arbitrary objects and '...' means zero or more occurence,
and false/true means a generalized boolean literal."
  (flet ((simplify-args (operator args)
           (iter (for arg in args)
                 (for simplified = (simplify-boolean-form arg))
                 (if (and (listp simplified) (eq (first simplified) operator))
                     (appending (cdr simplified))
                     (collect simplified)))))
    (case (and (listp form)
               (first form))
      (not
       (bind ((arg (simplify-boolean-form (first (cdr form)))))
         (cond ((and (listp arg)
                     (eq 'not (first arg)))
                (second arg))
               ((eq #f arg)
                #t)
               ((eq #t arg)
                #f)
               (t form))))
      (or
       (bind ((operands (remove #f (simplify-args 'or (cdr form)))))
         (cond
           ((null operands) #f)
           ((length= 1 operands) (first operands))
           ((some [eq !1 #t] operands) #t)
           (t `(or ,@operands)))))
      (and
       (bind ((operands (remove #t (simplify-args 'and (cdr form)))))
         (cond
           ((null operands) #t)
           ((length= 1 operands) (first operands))
           ((some [eq !1 #f] operands) #f)
           (t `(and ,@operands)))))
      (t
       form))))
