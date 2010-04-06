;;;
;;; Copyright (c) 2008-2010, Keith James.
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;     * Redistributions of source code must retain the above
;;;       copyright notice, this list of conditions and the following
;;;       disclaimer.
;;;
;;;     * Redistributions in binary form must reproduce the above
;;;       copyright notice, this list of conditions and the following
;;;       disclaimer in the documentation and/or other materials
;;;       provided with the distribution.
;;;
;;;     * Neither the names of the copyright holders nor the names of
;;;       its contributors may be used to endorse or promote products
;;;       derived from this software without specific prior written
;;;       permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
;;; TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
;;; THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;

(defpackage :uk.co.deoxybyte-systems
  (:use #:common-lisp)
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
