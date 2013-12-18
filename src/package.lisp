;;;
;;; Copyright (c) 2008-2013 Keith James. All rights reserved.
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
                #:defsystem
                #:module
                #:component
                #:component-pathname
                #:component-depends-on
                #:system-relative-pathname
                #:input-files
                #:source-file-type
                #:static-file
                #:cl-source-file
                #:operation
                #:operate
                #:operation-done-p
                #:find-system
                #:compile-system
                #:test-system
                #:perform
                #:compile-op
                #:load-op
                #:test-op)
  (:export
   #:compile-system
   #:load-system
   #:test-system
   #:document-system

   #:maybe-run-lift-tests
   #:maybe-build-cldoc-docs)
  (:documentation "The deoxybyte-systems system provides tools for
managing Common Lisp systems in batch mode and from the REPL. It
provides convenient wrapper functions for ASDF operations and collects
my ASDF extensions in one place, instead of copy-pasting them into the
system definition files that need them. It does not add symbols to
the ASDF package.

This system is required by most of my other Common Lisp systems
because it supplies the unit test and documentation support (using
LIFT {http://common-lisp.net/project/lift/} and CLDOC
{http://common-lisp.net/project/cldoc/} respectively.

This system requires ASDF 3 and will not function correctly with older
ASDF versions. One new ASDF operation is provided by this system:

- cldoc-op: Runs CLDOC to extract docstrings in HTML format.

Two utility functions are provided to allow LIFT tests to be run and
CLDOC documentation to be generated if LIFT and/or CLDOC are present,
degrading gracefully if they are not.

To use these in an ASDF file, first load these extensions. One way to
do this is to use a method specific to your Lisp platform (e.g. Steel
bank Common Lisp provides the .sbclrc file):

;;; (in-package :cl-user)
;;; (require 'asdf)
;;; (asdf:operate 'asdf:load-op :deoxybyte-systems)

Alternatively, the system can be loaded on demand by the ASDF file
that will use :deoxybyte-systems functions:

;;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;;  (when (asdf:find-system :deoxybyte-systems nil)
;;;    (asdf:load-system :deoxybyte-systems)))

;;; or loaded unconditionally:

;;; (in-package :cl-user)
;;; (asdf:load-system :deoxybyte-systems)

Then use standard ASDF syntax to add new components:

;;; (defsystem example-system
;;;   :name \"Example system\"
;;;   :in-order-to ((test-op (load-op :example-system
;;;                                   :example-system-test))
;;;                 (doc-op (load-op :cldoc)))
;;;   :components ((:module :src
;;;                         :serial t
;;;                         :pathname \"src/\"
;;;                         :components ((:file \"package.lisp\")
;;;                                      (:file \"example.lisp\")))
;;;   :perform (test-op :after (op c)
;;;                     (maybe-run-lift-tests :example-system
;;;                                           \"example-lift-test.config\"))
;;;   :perform (doc-op :after (op c)
;;;                    (maybe-build-cldoc-docs :example-system \"doc/html\")))

To load a system:
;;; (dxs:load-system :example-system)

or the equivalent:
;;; (asdf:load-system :example-system)


To run tests:
;;; (dxs:test-system :example-system)

or the equivalent:
;;; (asdf:test-system :example-system)

To generate system documentation:
;;; (dxs:document-system :example-system)"))
