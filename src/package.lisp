;;;
;;; Copyright (C) 2008-2009 Keith James. All rights reserved.
;;;
;;; This file is part of deoxybyte-systems.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

(defpackage :uk.co.deoxybyte-systems
  (:use #:common-lisp #:cldoc)
  (:nicknames
   #:deoxybyte-systems
   #:dxs)
  (:import-from #:asdf
                #:module
                #:component
                #:component-pathname
                #:input-files
                #:source-file-type
                #:static-file
                #:cl-source-file
                #:operation
                #:operate
                #:operation-done-p
                #:find-system
                #:perform
                #:compile-op
                #:load-op
                #:test-op)
  (:export
   #:lift-test-config
   #:cldoc-config

   #:compile-system
   #:load-system
   #:test-system
   #:document-system

   #:package-version-p
   #:package-version
   #:package-version=
   #:package-version>
   #:package-version>=
   #:package-version<
   #:package-version<=)
  (:documentation "The deoxybyte-systems system provides tools for
managing Common Lisp systems in batch mode and from the REPL. It
provides convenient wrapper functions for ASDF operations and collects
my ASDF extensions in one place, instead of copy-pasting them into the
system definition files that need them. It does not add symbols to
the ASDF package.
  
This system is required by most of my other Common Lisp systems
because it supplies the unit test and documentation
configurations (using LIFT {http://common-lisp.net/project/lift/} and
CLDOC {http://common-lisp.net/project/cldoc/} respectively.

Two new ASDF components are provided by this system:

- lift-test-config: Loads a LIFT test configuration file when an
  ASDF:TEST-OP operation is run.

- cldoc-config: Specifies a package in which to run CLDOC, thereby
  extracting the docstrings in HTML format.

One new ASDF operation is provided by this system:

- cldoc-op: Runs CLDOC to extract docstrings in HTML format.


To use these new components in an ASDF file, first load these
extensions (this may be omitted if you can be sure that the
extensions will be loaded in some other way):

;;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;;  (when (asdf:find-system :deoxybyte-systems nil)
;;;    (asdf:operate 'asdf:load-op :deoxybyte-systems)))

Then use standard ASDF syntax to add new components:

;;; (defsystem example-system
;;;   :name \"Example system\"
;;;   :components ((:module :src
;;;                         :serial t
;;;                         :pathname \"src/\"
;;;                         :components ((:file \"package.lisp\")
;;;                                      (:file \"example.lisp\")))
;;;                (:lift-test-config :lift-tests
;;;                                   :pathname \"example-test.config\"
;;;                                   :target-system :example-system)
;;;                (:cldoc-config :cldoc-documentation
;;;                               :pathname \"doc/html/\"
;;;                               :target-system :example-system)))

To load a system:
;;; (dxs:load-system :example-system)

To run tests:
;;; (dxs:test-system :example-system)

To generate system documentation:
;;; (dxs:document-system :example-system)"))
