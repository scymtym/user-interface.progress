;;;; protocol.lisp --- Protocol functions provided by the user-interface.progress system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:user-interface.progress)

;;; Condition handling protocol

(defgeneric handle (info condition)
  (:documentation
   "Update the progress information tracked in INFO based on
    CONDITION."))

;;; Reporting protocol

(defgeneric report (info target style &key &allow-other-keys)
  (:documentation
   "Write a report for INFO onto TARGET using STYLE.

    In many cases, TARGET will be a stream and STYLE will control the
    formatting of the report.

    When TARGET is a character stream, methods on this generic
    function should return the number of lines written to TARGET."))

;;; Info progress protocol

(defgeneric info-progress (info)
  (:documentation
   "Return the progress of the operation (or operations) tracked by
    INFO.

    If INFO contains children, the returned progress may be the mean
    progress of these children or some other reduced summary."))

(defgeneric info-finished? (info)
  (:documentation
   "Return non-nil if the operation for which INFO tracks progress is
    finished."))

;; Default behavior

(defmethod info-progress ((info t))
  nil)

(defmethod info-finished? ((info t))
  (eq (info-progress info) t))

;;; Info timing protocol

(defgeneric info-start-time (info)
  (:documentation
   "Return the start of INFO as time in seconds since an arbitrary
    point in time determined by `get-internal-real-time'."))

(defgeneric info-duration (info)
  (:documentation
   "Return the time for which the operation tracked by INFO has been
    executing in seconds."))

;;; Info children protocol

(defgeneric info-children (info)
  (:documentation
   "Return a list of the \"inferior\" info objects of INFO."))

;;; Thread info protocol

(defgeneric info-remove-finished (info)
  (:documentation
   "Remove children corresponding to finished operations from INFO."))

;;; Operation info protocol

(defgeneric info-operation (info)
  (:documentation
   "Return the operation for which INFO tracks the progress."))
