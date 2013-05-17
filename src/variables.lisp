;;;; variables.lisp --- Variables used by the user-interface.progress system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:user-interface.progress)

(declaim (special *info* *info-lock*))

(defvar *info* (make-instance 'global-progress-info)
  "TODO")

(defvar *info-lock* (bt:make-lock)
  "TODO(jmoringe): document")
