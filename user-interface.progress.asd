;;;; user-interface.progress.asd --- System definition for user-interface.progress.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:user-interface.progress-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:user-interface.progress-system)

;;; Version stuff

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 1
  "Minor component of version number.")

(defparameter +version-revision+ 0
  "Revision component of version number.")

(defun version/list ()
  "Return a version of the form (MAJOR MINOR REVISION)."
  (list +version-major+ +version-minor+ +version-revision+))

(defun version/string ()
  "Return a version string of the form \"MAJOR.MINOR.REVISION\"."
  (format nil "~{~A.~A.~A~}" (version/list)))

;;; System definition

(defsystem :user-interface.progress
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "Progress reporting, primarily for terminal applications."
  :depends-on  (:alexandria
                :iterate
                (:version :let-plus        "0.2")
                (:version :more-conditions "0.4")
                :bordeaux-threads

                :terminal.ansi-escapes)
  :components  ((:module     "src"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "util")
                              (:file       "protocol")
                              (:file       "info-mixins")
                              (:file       "info")
                              (:file       "report-mixins")
                              (:file       "report")
                              (:file       "variables")
                              (:file       "macros"))))

  :in-order-to ((test-op (test-op :user-interface.progress-test))))

(defsystem :user-interface.progress-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "Unit tests for the user-interface.progress system."
  :depends-on  ((:version :user-interface.progress #.(version/string))
                :eos)
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "conditions")
                              (:file       "macros")))))

(defmethod perform ((op        test-op)
                    (component (eql (find-system :user-interface.progress-test))))
  (funcall (find-symbol "RUN-TESTS" :user-interface.progress.test)))
