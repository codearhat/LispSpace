;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

#|
to introduce structure-persistent-object without identity being flattened into its owner's table

rename persistent-class to standard-persistent-class
chop slot-persistent-class from persistent-class  
define structure-persistent-class

(defclass slot-persistent-class (standard-class)
  ())

(defclass structure-persistent-class (slot-persistent-class)
  ())

(defclass standard-persistent-class (slot-persistent-class)
  ())

rename persistent-object to standard-persistent-object
chop slot-persistent-object from persistent-object
define structure-persistent-object

(defclass slot-persistent-object (standard-object)
  (cached-slot))

(defclass structure-persistent-object (slot-persistent-object)
  ((parent :type standard-persistent-object)
   (slot :type standard-persistent-effective-slot-definition)))

(defclass standard-persistent-object (slot-persistent-object)
  ())

rename/chop/define structure-slot-definitions?!!!!

this is possible

(defclass abstract-address (slot-persistent-object)
  ((a :type boolean))
  (:metaclass slot-persistent-class))

(defclass address-structure (abstract-address)
  ()
  (:metaclass structure-persistent-class))

(defclass address (abstract-address)
  ()
  (:metaclass standard-persistent-class))

(defpclass user ()
  ((abstract-address :type abstract-address) ; error 
   (address :type address)              ; reference 
   (address-structure :type address-structure) ; flattened
   ))

add svuc and friends
flatten columns into primary-table-of standard-persistent-class
refactor store to prefetch flattened columns and to be able to store structure slot values individually

plus query :) (poor tomi)
|#
