;;;; info-mixins.lisp --- Mixins for info classes.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:user-interface.progress)

;;; `timing-mixin' mixin class

(defclass timing-mixin ()
  ((start-time :initarg  :start-time
               :type     real
               :reader   info-start-time
               :documentation
               "Stores the start time of the operation described by
                the info object. The start time is stored in seconds
                and based on `get-internal-real-time'."))
  (:default-initargs
   :start-time (now/seconds))
  (:documentation
   "This class is intended to mixed into progress info classes which
    have to track the start times of operations and report durations
    of operations."))

(defmethod info-duration ((info timing-mixin))
  (- (now/seconds) (info-start-time info)))
