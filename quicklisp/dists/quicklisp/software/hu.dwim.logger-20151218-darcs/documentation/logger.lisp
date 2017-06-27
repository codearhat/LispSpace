;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.logger.documentation)

(def project :hu.dwim.logger)

(def book user-guide (:title "User guide for hu.dwim.logger")
  (chapter (:title "Introduction")
    (paragraph ()
      "A logging library that, among other things, supports chaining log messages through a hierarchy of loggers, separate compile-time and runtime log level, and provides various appenders, including one that supports multiple threads.")
    (paragraph ()
      "It's based on the logger in " (hyperlink "http://common-lisp.net/project/bese/" "Arnesi") ", originally written by Marco Baringer."))
  (chapter (:title "Tutorial")
    ;; TODO add links to code fragments
    (paragraph ()
      "Take the following example logger definition:
\(def logger my-logger ()
  :runtime-level +info+
  :compile-time-level +debug+
  :appenders ((make-thread-safe-stream-appender '*standard-output*)))

It will define 6 macros, each called MY-LOGGER.{level} for each of the 6 log levels. These macros act similarly to CL:FORMAT with a few exceptions:
 - they don't take a first destination argument
 - they implicitly append a leading ~% (newline)
 - they have an implicit error guard that prevents errors coming out of them
 - they conditionally print their messages based on the logger's current runtime level
 - at compile time they may macroexpand to NIL if the compile time level of their logger is set something higher than them

The log messages are handled by the so called appenders. Each logger can have a list of appenders. Through the parent chain of the logger, each appender of each parent also receives the log message.

The logger called ROOT-LOGGER is implicitly becomes a parent of all loggers, so having a single appender on it catches all log messages. If the codebase is loaded with debug settings (see the global varaible called *LOAD-AS-PRODUCTION?*) then it will have a (MAKE-THREAD-SAFE-STREAM-APPENDER '*STANDARD-OUTPUT*).

STREAM-APPENDER is a simple stream appender that writes to its target stream, *STANDARD-OUTPUT* by default.

THREAD-SAFE-FILE-APPENDER can be used from multiple threads because it caches messages protected by a lock and can flush messages from a timer (also protected by a lock, so the file contents will not be cluttered by overlapping writes from multiple threads).")))
