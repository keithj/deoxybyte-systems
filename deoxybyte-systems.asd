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

(in-package :cl-user)

(defpackage :uk.co.deoxybyte-systems-system
  (:use :common-lisp :asdf))

(in-package :uk.co.deoxybyte-systems-system)

(defsystem deoxybyte-systems
  :name "Deoxybyte Common Lisp system utilities"
  :author "Keith James"
  :licence "GPL v3"
  :depends-on (:lift :cldoc :cl-fad)
  :components ((:module :deoxybyte-systems
                        :serial t
                        :pathname "src/"
                        :components ((:file "package")
                                     (:file "asdf-extensions")
                                     (:file "deoxybyte-systems")))))
