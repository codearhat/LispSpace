;;;; -*- Mode: Lisp -*-
;;;; $Id: scl.lisp 623 2008-11-14 17:24:33Z binghe $

(in-package :snmp)

(def-scalar-variable "sysObjectID" (agent)
  (oid "clNetSnmpAgentSCL"))
