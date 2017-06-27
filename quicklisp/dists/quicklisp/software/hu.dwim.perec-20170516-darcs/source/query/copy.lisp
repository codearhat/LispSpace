;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

(define-copy-protocol copy-query)

(define-copy-method (copy-one copy-query) ((class persistent-class) htable)
  class)

(define-copy-method (copy-one copy-query) ((object persistent-object) htable)
  object)

(define-copy-method (copy-one copy-query) ((local-time timestamp) htable)
  local-time)

(define-copy-method (copy-one copy-query) ((slot persistent-slot-definition) htable)
  slot)

(define-copy-method (copy-one copy-query) ((struct structure-object) htable)
  struct)

(define-copy-protocol copy-shallow)

(define-copy-method (copy-one copy-shallow) ((class persistent-class) htable)
  class)

(define-copy-method (copy-one copy-shallow) ((object persistent-object) htable)
  object)

(define-copy-method (copy-one copy-shallow) ((local-time timestamp) htable)
  local-time)

(define-copy-method (copy-one copy-shallow) ((slot persistent-slot-definition) htable)
  slot)

(define-copy-method (copy-one copy-shallow) ((struct structure-object) htable)
  struct)
