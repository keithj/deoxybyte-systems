Introduction

The deoxybyte-systems system provides tools for managing Common Lisp
systems in batch mode and from the REPL. It provides convenient
wrapper functions for ASDF operations and collects my ASDF extensions
in one place, instead of copy-pasting them into the system definition
files that need them. It does not add symbols to the ASDF package.

This system is required by most of my other Common Lisp systems
because it supplies the optional unit test and documentation
configurations (using LIFT <http://common-lisp.net/project/lift/> and
CLDOC <http://common-lisp.net/project/cldoc/> respectively.


Documentation

See the Lisp docstrings, particularly the package docstrings for an
overview. HTML documentation may be generated with the command:

  (cldoc:extract-documentation 'cldoc:html "./doc/html/"
    (find-system :deoxybyte-systems))

at the REPL, provided that CLDOC is installed.


Dependencies

ASDF    http://common-lisp.net/project/asdf/  (Requires ASDF 3)
cl-fad  http://www.weitz.de/cl-fad/

Optional dependencies

LIFT    http://common-lisp.net/project/lift/
CLDOC   http://common-lisp.net/project/cldoc/
