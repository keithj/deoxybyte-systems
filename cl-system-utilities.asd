 ;;;
;;; Copyright (C) 2008 Keith James. All rights reserved.
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

(in-package :cl-user)

(defpackage #:cl-system-utilities-system
  (:use :common-lisp :asdf))

(in-package :cl-system-utilities-system)

(defsystem cl-system-utilities
  :name "Common Lisp system utilities"
  :author "Keith James"
  :licence "GPL v3"
  :depends-on (:lift :cldoc :cl-dot)
  :components ((:module :cl-system-utilities
                        :serial t
                        :pathname "src/"
                        :components ((:file "package")
                                     (:file "asdf-extensions")
                                     (:file "cl-system-utilities")))))
