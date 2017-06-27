;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;;
;;;; Steps to prepare your database for free text search with hu.dwim.perec using PostgreSQL
;;;;
;;;; 1. Install postgresql-contrib module using your package manager (this will provide pg_trgm)
;;;;    sudo aptitude install postgresql-contrib
;;;; 2. Install pg_trgm package into your database
;;;;    sudo su - postgres
;;;;    psql -d <database-name> -f /usr/share/postgresql/8.4/contrib/pg_trgm.sql
;;;; 3. Install tsearch package into your database
;;;;    sudo su - postgres
;;;;    psql -d <database-name> -f /usr/share/postgresql/8.4/contrib/tsearch2.sql
;;;; 4. Rebuild the text search index every now and then
;;;;    (rebuild-text-search-index)
;;;; 5. Do text searches
;;;;    (text-search-instances ...)
;;;;
;;;; TODO: FIXME: use query variable binding

;;;;;;;
;;; rebuild-text-search-index

(def (special-variable) *text-search-configuration* "simple")

(def function text-search-class-name-provider (class)
  (substitute #\Space #\- (string-downcase (class-name class))))

(def (function e) rebuild-text-search-index (&key (configuration *text-search-configuration*) (class-name-provider #'text-search-class-name-provider))
  (check-type *database* hu.dwim.rdbms.postgresql:postgresql)
  (with-transaction
    (ignore-errors (execute "DROP TABLE _text_search_object")))
  (with-transaction
    (execute (format nil "CREATE TABLE _text_search_object AS ~{~A~^ UNION ALL ~}"
                     (iter (for (name class) :in-hashtable *persistent-classes*)
                           (for class-names = (mapcar class-name-provider
                                                      ;; NOTE: leave out persistent-object
                                                      (butlast (hu.dwim.perec::persistent-class-precedence-list-of class))))
                           (for direct-instances-prefetch-view = (direct-instances-prefetch-view-of class))
                           (when direct-instances-prefetch-view
                             (when-bind column-names
                                 (iter slot-loop
                                       (for slot :in (prefetched-slots-of class))
                                       (when (subtypep (canonical-type-of slot) '(or h-unused unbound null string))
                                         (iter column-loop
                                               (for column :in (columns-of slot))
                                               (when (typep (hu.dwim.rdbms::type-of column) 'hu.dwim.rdbms::sql-string-type)
                                                 (in slot-loop (collect (hu.dwim.rdbms::name-of column)))))))
                               (collect (format nil "SELECT _oid AS _oid, to_tsvector('~A', '~{~A ~}' || ~{coalesce(~A,'')~^ || ~}) AS _tsv FROM ~A"
                                                configuration class-names column-names (name-of direct-instances-prefetch-view))))))))
    (execute "CREATE INDEX _text_search_object_idx ON _text_search_object USING gin(_tsv)")
    (execute "ANALYZE _text_search_object"))
  (with-transaction
    (ignore-errors (execute "DROP TABLE _text_search_dictionary")))
  (with-transaction
    (execute "CREATE TABLE _text_search_dictionary AS SELECT word AS _word FROM stat('SELECT _tsv FROM _text_search_object')")
    ;; NOTE: we must ensure that the class names are in the dictionary literally
    (iter (for (name class) :in-hashtable *persistent-classes*)
          (for class-name = (funcall class-name-provider class))
          (unless (length= 1 (execute (format nil "SELECT 1 FROM _text_search_dictionary WHERE _word = '~A'" class-name)))
            (execute (format nil "INSERT INTO _text_search_dictionary VALUES ('~A')" class-name))))
    (execute "CREATE INDEX _text_search_dictionary_ids ON _text_search_dictionary USING gist(_word gist_trgm_ops);")
    (execute "ANALYZE _text_search_dictionary")))

;;;;;;;
;;; text-search-instances

(def (function e) text-search-dictionary (word &key offset limit threshold)
  (set-text-search-threshold threshold)
  (execute (make-text-search-dictionary-query word :offset offset :limit limit)))

(def (function e) text-search-instances (text &key (configuration *text-search-configuration*) (fuzzy #t) offset limit threshold)
  ;; TODO: support AND/OR/NOT using to_tsquery instead of plainto_tsquery
  ;; TODO: need to make a parser for that
  ;; TODO: when using fuzzy 'foo and not bar' must be turned into '(foo1 | foo2) & !(bar1 | bar2)
  ;; TODO: but how do we get the correct score then?
  (set-text-search-threshold threshold)
  (when fuzzy
    (create-fuzzy-text-search-temporary-table text :limit limit))
  (prog1
      (map 'list [load-instance (first-elt !1) :skip-existence-check #t]
           (execute (make-text-search-instances-query text :configuration configuration :fuzzy fuzzy :offset offset :limit limit)))
    (when fuzzy
      (drop-table "_text_search_text"))))

(def function create-fuzzy-text-search-temporary-table (text &key limit)
  (bind ((words (split #\Space text))
         (table-names (iter (for index :from 0)
                            (for word :in words)
                            (for table-name = (format nil "_text_search_word~A" index))
                            (execute (format nil "CREATE TEMPORARY TABLE ~A AS ~A" table-name (make-text-search-dictionary-query word :limit limit)))
                            (collect table-name)))
         (word-column-names (iter (for table-name :in table-names)
                                  (collect (format nil "~A._word" table-name))))
         (similarity-column-names (iter (for table-name :in table-names)
                                        (collect (format nil "~A._similarity" table-name)))))
    ;; TODO: how do we combine score better than this?
    (execute (format nil "
CREATE TEMPORARY TABLE _text_search_text AS
SELECT ~{~A~^ || ' ' || ~} AS _text, ~{~A~^ * ~} AS _similarity
FROM ~{~A~^, ~}
~A" word-column-names similarity-column-names table-names (make-limit-clause limit)))
    (foreach 'drop-table table-names)))

(def function make-text-search-dictionary-query (word &key offset limit)
  (format nil "
SELECT _word, similarity(_word, '~A') AS _similarity
FROM _text_search_dictionary
WHERE _word % '~A'
ORDER BY _similarity DESC, _word
~A ~A" word word (make-offset-clause offset) (make-limit-clause limit)))

(def function make-text-search-instances-query (text &key (configuration *text-search-configuration*) (fuzzy #t) offset limit)
  ;; TODO: some views could help about this
  (if fuzzy
      ;; TODO: how do we combine score better than this?
      (format nil "
SELECT _inner_select.*, _rank * _similarity AS _rank_similarity
FROM (SELECT _oid, max(ts_rank_cd(_tsv, plainto_tsquery('~A', _text_search_text._text))) AS _rank, max(_text_search_text._similarity) AS _similarity
      FROM _text_search_object, _text_search_text
      WHERE _tsv @@ plainto_tsquery('~A', _text_search_text._text)
      GROUP BY _oid) AS _inner_select
ORDER BY _rank_similarity DESC
~A" configuration configuration (make-limit-clause limit))
      (format nil "
SELECT _oid, ts_rank_cd(_tsv, _query) AS _rank
FROM _text_search_object, (SELECT plainto_tsquery('~A', '~A') AS _query) AS _test_search_query
WHERE _tsv @@ _query
ORDER BY _rank DESC
~A ~A" configuration text (make-limit-clause offset) (make-limit-clause limit))))

(def function make-offset-clause (offset)
  (if offset
      (format nil "OFFSET ~A" offset)
      ""))

(def function make-limit-clause (limit)
  (if limit
      (format nil "LIMIT ~A" limit)
      ""))

(def function set-text-search-threshold (threshold)
  (when threshold
    (execute (format nil "SELECT set_limit(~A)" threshold))))
