;;;; package.lisp --- Package definition for the user-interface.progress system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:user-interface.progress
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:more-conditions
   #:bordeaux-threads)

  ;; Condition handling protocol
  (:export
   #:handle)

  ;; Reporting protocol
  (:export
   #:report)

  ;; Info progress protocol
  (:export
   #:info-progress
   #:info-finished?)

  ;; Info timing protocol
  (:export
   #:info-start-time
   #:info-duration)

  ;; Info children protocol
  (:export
   #:info-children)

  ;; Thread info protocol
  (:export
   #:info-operations)

  ;; Operation info protocol
  (:export
   #:info-operation)

  ;; Macros
  (:export
   #:with-progress-report)

  (:documentation
   "TODO"))
