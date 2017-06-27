Plain-odbc is lisp wrapper around the ODBC library.
It works on UNIX on Windows.

Ir depends on the CFFI system.
CFFI itself depends on the systems
babel
alexandria
trivial-features,

These systems are included in the directory libs.

The file "plain-odbc-with-libs.asd" contains the code that will load the
system definitions for the included libraries and define the system
plain-odbc-with-libs which huts depends on the system plain-odbc.

(asdf:oos 'asdf:load-op :plain-odbc-with-libs)

will load plain-odbc and the included libraries.

If you have your own version of these libraries you can still use
the system file "plain-odbc.asd"



