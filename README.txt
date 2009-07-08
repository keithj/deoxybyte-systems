Introduction

The deoxybyte-systems system provides tools for managing Common Lisp
systems in batch mode and from the REPL. It provides convenient
wrapper functions for ASDF operations and collects my ASDF extensions
in one place, instead of copy-pasting them into the system definition
files that need them. It does not add symbols to the ASDF package.
  
This system is required by most of my other Common Lisp systems
because it supplies the unit test and documentation configurations
(using LIFT <http://common-lisp.net/project/lift/> and CLDOC
<http://common-lisp.net/project/cldoc/> respectively.


Documentation

See the Lisp docstrings, particularly the package docstrings for an
overview.


Dependencies

ASDF    http://common-lisp.net/project/asdf/
LIFT    http://common-lisp.net/project/lift/
CLDOC   http://common-lisp.net/project/cldoc/
cl-fad  http://www.weitz.de/cl-fad/
