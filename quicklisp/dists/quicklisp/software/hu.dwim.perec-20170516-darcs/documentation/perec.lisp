;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.documentation)

(def project :hu.dwim.perec
  :description (book ()
                 (chapter (:title "Introduction")
                   (paragraph ()
                     "The class metaobject called persistent-class in cl-perec extends the CLOS standard-class and allows classes to have persistent slots. In fact defpclass is nothing more than a defclass with a (:metaclass persistent-class). In a persistent class all slots are persistent by default unless the :persistent nil slot option is specified in which case the slot will be a standard slot. The persistent-class implements a subset of the CL type system and maps slots to RDBMS tables and columns. For several primitive types this can be done efficiently while some CL types need to be serialized and stored as BLOB in the RDBMS.")
                   (paragraph ()
                     "The intention of cl-perec is to completely hide the RDBMS part from the user. It creates and alters tables automatically (signals continuable errors for schema changes) so the user can mostly think in CLOS terms.")
                   (paragraph ()
                     "There is another class metaobject called persistent-association which allows two slots in two different classes to be in referential integrity. There are three different kind of associations: one-to-one, one-to-many and many-to-many. The many association end means an unordered set of persistent objects.")
                   (paragraph ()
                     "Another nice part is the query compiler which provides a convenient way to express queries in the lisp type system which is then mapped to efficient SQL queries if possible. Even if the entire query cannot be mapped to a single SQL statement, parts of it will transparently be run in the Lisp VM. Ordering, slot accessors, literals and lisp predicates can be used to build the query, see the huge number of tests for more details.")
                   (paragraph ()
                     "The persistent type system supports a subset of the Common Lisp type system, type aliasing and subtyping are transparently mapped to RDBMS. New primitive persistent types can be defined with defptype and defmapping. The extended metaobject protocol allows mapping lisp types to multiple RDBMS columns (e.g. the type (or null unbound integer) needs two columns)."))
                 (chapter (:title "Features")
                   (paragraph ()
                     (list "supported extensible primitive types: t, serialized, unbound, null, boolean, integer, integer-16, integer-32, integer-64, float, double, number, string, (string exact-size), text, (text size-limit), symbol, (symbol* size-limit) date, time, timestamp, member, duration, form, unsigned-byte-vector, ip-address"
                           "limited or type support (or null ...) and unbound slot support (can be specified with (or unbound ...))"
                           "persistent classes with CLOS multiple inheritence"
                           "lazy slot access both for primitive types and class reference types with prefetch support for performance finetuning"
                           "lazy persistent sets (with and without identity)"
                           "one-to-one, one-to-many and many-to-many associations that maintain referential integrity"
                           "ACID transactions and concurrency inherited from the underlying RDBMS, nested transaction support"
                           "efficient object and slot level caching with transaction isolation"
                           "expressive query language (possibly partially) compiled into SQL"
                           "the query compiler understands polimorphism, associations and the persistent type system"
                           "SQL queries can still be used if needed and lazy slot access and navigation will work automatically"
                           "limited schema evolution, transparent additive changes"
                           "support for destructive schema changes (see the confirmation example)"
                           "time machine (tesites) to be able to sepcify slot values in terms of timestamps and time intervalls")))))

(def method make-project-tab-pages ((component project/detail/inspector) (project (eql (find-project :hu.dwim.perec))))
  (append (call-next-method)
          (list (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "User guide"))
                  (make-value-inspector (find-book 'user-guide))))))

(def book user-guide (:title "User guide")
  (chapter (:title "Introduction")
    (chapter (:title "What is hu.dwim.perec")
      (paragraph ()
        "There's a blog post about an older (but not fundamentally different) version of perec: " (hyperlink "http://pinterface.livejournal.com/34706.html")))
    (chapter (:title "Why not something else?")
      ))
  (chapter (:title "Supported Platforms")
    (chapter (:title "Operating Systems")
      )
    (chapter (:title "Common Lisp Implementations")
      ))
  (chapter (:title "Instal Guide"))
  (chapter (:title "Connect to Database"))
  (chapter (:title "Tutorial")
    (chapter (:title "Persisting Objects")
      ))
  (chapter (:title "Concepts")
    (chapter (:title "Database")
      )
    (chapter (:title "Transaction")
      )
    (chapter (:title "Persistent Objects")
      )
    (chapter (:title "Transient Objects")
      )
    (chapter (:title "Persistent Types")
      )
    (chapter (:title "Type Mapping")
      )
    (chapter (:title "Cache")
      )
    (chapter (:title "Export")
      (paragraph ()
        "Export is the mechanism which syncronizes the relational database schema with the persistent class and association meta objects. In a single threaded environment you can rely on automatic export. In a multi threaded environment you have to explicitily call " (find-function 'ensure-exported) " before using your persistent classes."))
    (chapter (:title "Query Lanugage")
      )
    (chapter (:title "Query Compiler")
      )
    (chapter (:title "Multi Dimensional Values")
      )))



#|
-*- outline -*-

* hu.dwim.perec: an Object Relational Mapping (ORM) from CLOS to RDBMS

** Environment for the test suite (using the PostgreSQL backend)

Create the database and user:

$ createdb hu.dwim.perec.test
$ createuser -P hu.dwim.perec.test

Give "engedjbe" as password.

** Running the tests using the PostgreSQL backend

(asdf:test-system :hu.dwim.perec.postgresql)

or use 'asdf:load-op to merely load it.

** Some simple examples

(defpclass foo ()
  ((bar :type integer-16)))

(with-transaction
  (make-instance 'foo :bar 1))

(with-transaction
  (select (i)
    (from (i foo))
    (where (= 1 (bar-of i)))))

** Loading the system without the test environment

(load-system :hu.dwim.perec)

For some details on how to set up the PostgreSQL specific configuration see the hu.dwim.perec.test.postgresql.asd file.
|#

#| ;; TODO:
Columns
=======
Status Priority Description

Status
======
- todo
+ done

Priorities
==========
0 - highest
1 - high
2 - normal
3 - low
4 - lowest

Entries
=======
use M-x sort-lines to keep the order

-0 subclassing a dimensional class does not make the subclass dimensional if there are no dimensional slots in the subclass
-0 lichteblau: For queries like (eql (prc:oid-of table) 123), perec infers the type of 123 as NUMERIC (not BIGINT), meaning that PostgreSQL does a sequential scan on the ID column.
-1 change-class, add-class, remove-class, ensure-class, desure-class (support anonymous persistent classes)
-1 add delayed updates using mark dirty-slots and provide a flush function
-1 revise perec persistent-p/debug-persistent-p
-2 add asserts in update/select statements for the number of affected rows in store
-2 prefetching 1-1 association does not always work due to not knowing that both ends are persistent (why do we assume broken references there) and thus executing a query upon persistent-p
-3 finish sqlite support (issues: there are no sequences; no reflection on the column level; no date, time, timestamp data type)
-3 what about purge-instance making references broken?
-4 eliminate barely used (by increasing duplicates) system and package dependencies
-4 eliminate superfluous duplicates (which are covered by other systems)

   clean up condition inheritance, introduce a common base class for all perec conditions



the default value handling of dimensional slots is not totally correct:
(some-slot 0 :type integer) -> unless +whole-domain-marker+ is used at make-instance, there'll be unbound ranges in the d-value.
resolution: remove premature optimization and always insert the whole-domain-markers no matter what.


BUG: the coordinate 'cooking' in make-d-value is broken:
 - sometimes the conses come out of it
 - sometimes it dies due to something is not of type cons



broken optimization: when setf'ing a dimensional slot, we have the chance to update a row instead of inserting a new one.
the current try is pretty much useless due to MAX(_h_instance_2._oid). a more effective optimization is very complex.

this might be worth considering:
  - when a subcube is set
  - and there's a row that exactly matches the coordinates
  - and there are no other rows that overlap the coordinates with a bigger oid
  then that row can be updated to the new value.

$1 = 0 as SMALLINT, $2 = 5880000 as NUMERIC, $3 = 210349740372 as BIGINT, $4 = 210349740372 as BIGINT, $5 = 210369796166 as BIGINT, $6 = 210369796166 as BIGINT, $7 = NIL as BOOL, $8 = 210369796166 as BIGINT, $9 = 2010-01-01T01:00:00.000000+01:00 as TIMESTAMP WITH TIME ZONE, $10 = 2011-01-01T01:00:00.000000+01:00 as TIMESTAMP WITH TIME ZONE, $11 = 302785128137 as BIGINT, $12 = 302785128137 as BIGINT, $13 = NIL as BOOL, $14 = 302785128137 as BIGINT, $15 = 210349126661 as BIGINT, $16 = 210349126661 as BIGINT, $17 = NIL as BOOL, $18 = 210349126661 as BIGINT, $19 = 210349740372 as BIGINT, $20 = 210349740372 as BIGINT
UPDATE _támogatási_kimutatás_h SET _támogatási_összeg_tag = $1::SMALLINT,
                                   _támogatási_összeg = $2::NUMERIC
WHERE (_támogatási_kimutatás_h._oid IN (SELECT _h_instance._oid FROM _támogatási_kimutatás_h_ap _h_instance
                                        WHERE (((NOT ($3::BIGINT IS NULL)) AND
                                                (_h_instance._d_instance_oid = $4::BIGINT)) AND
                                               (CASE WHEN ((_h_instance._kimutatási_alany_oid IS NULL) OR ($5::BIGINT IS NULL)) THEN (CASE WHEN ((_h_instance._kimutatási_alany_oid IS NULL) AND ($6::BIGINT IS NULL)) THEN (2 = 2) ELSE $7::BOOL END) ELSE (_h_instance._kimutatási_alany_oid = $8::BIGINT) END) AND
                                               ((_h_instance._validity_begin = $9::TIMESTAMP WITH TIME ZONE)) AND
                                               ((_h_instance._validity_end = $10::TIMESTAMP WITH TIME ZONE)) AND
                                               (CASE WHEN ((_h_instance._felmérés_oid IS NULL) OR ($11::BIGINT IS NULL)) THEN (CASE WHEN ((_h_instance._felmérés_oid IS NULL) AND ($12::BIGINT IS NULL)) THEN (2 = 2) ELSE $13::BOOL END) ELSE (_h_instance._felmérés_oid = $14::BIGINT) END) AND
                                               (CASE WHEN ((_h_instance._támogatási_jogcím_oid IS NULL) OR ($15::BIGINT IS NULL)) THEN (CASE WHEN ((_h_instance._támogatási_jogcím_oid IS NULL) AND ($16::BIGINT IS NULL)) THEN (2 = 2) ELSE $17::BOOL END) ELSE (_h_instance._támogatási_jogcím_oid = $18::BIGINT) END) AND
                                               (_h_instance._oid = (SELECT MAX(_h_instance_2._oid)
                                                                    FROM (SELECT _oid, _támogatási_kimutatás_h._d_instance_oid
                                                                          FROM _támogatási_kimutatás_h)
                                                                         _h_instance_2
                                                                    WHERE (((NOT ($19::BIGINT IS NULL)) AND
                                                                            (_h_instance_2._d_instance_oid = $20::BIGINT))))))))
|#











#|
Testbed default parameters (port is set to PostgreSQL default port):
   host: localhost
   port: 5432
   database: hu.dwim.perec.test
   user-name: hu.dwim.perec.test
   password: engedjbe

To install postgresql:
   sudo apt-get install postgresql

To setup the test database:
   sudo su - postgres
   createdb hu.dwim.perec.test
   createuser -d -r -l -P hu.dwim.perec.test
   ;; type in 'engedjbe' for password

In emacs do:
   ;; the swank server uses utf-8, so
   M-S-: (setq slime-net-coding-system 'utf-8-unix)
   M-x slime-connect
   ;; 'localhost' and default port 4005 should be ok

To test hu.dwim.perec:
   (in-package :hu.dwim.perec.test) ; this is the default when you connect
   (retest) ; should print a lot of dots and stuff and takes a while

To play around:
   ;; to turn on logging of SQL statements in SLIME
   (enable-sql-log)
   ;; to create a persistent class
   (defpclass* test ()
     ((name :type (text 20))
      (age :type integer-32)
      (flag :type boolean)))
   ;; to make an instance
   ;; this should automatically create/update the tables needed for the class
   ;; note: if you have run the test suite, this might execute several queries
   ;;       to check all persistent classes present in your lisp image
   (defvar p
     (with-transaction
        (make-instance 'test :name \"Hello\" :age 42 :flag t)))
   ;; to reuse the instance in another transaction
   (with-transaction
     (with-revived-instance p
       (describe p)))
   ;; to query instances of the class just defined
   (with-transaction
     (select (instance)
       (from (instance test))
       (where (and (equal (name-of instance) \"Hello\")
                   (< (age-of instance) 100)))
       (order-by :descending (age-of instance))))
   ;; queries are polimorph by default (this should actually return all persistent instances)
   ;; use macroexpand to see how it compiles down to straight SQL
   (with-transaction
     (select (:compile-at-macroexpand t) (instance)
       (from (instance persistent-object))))
   ;; see the tests in the repository at http://common-lisp.net/cgi-bin/darcsweb/darcsweb.cgi?r=hu.dwim.perec-hu.dwim.perec;a=tree;f=/test
   ;; see a somewhat more complicated example at: http://common-lisp.net/project/hu.dwim.perec/shop.html
   ;; and also check the showcase on the website at http://common-lisp.net/project/hu.dwim.perec/showcase.html

To read more about the project:
   http://common-lisp.net/project/hu.dwim.perec

There is some form of documentation at:)
   http://common-lisp.net/project/hu.dwim.perec/documentation/index.html

Suggestions, bug reports are welcomed at:
   hu.dwim.perec-devel@common-lisp.net
|#
