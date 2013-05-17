;;;; macros.lisp --- Macros provided by the user-interface.progress system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:user-interface.progress)

(defun invoke-with-handler (update handle-condition thunk)
  "Setup HANDLE-CONDITION to be called with progress conditions
   signaled by THUNK. Call UPDATE after THUNK returns or non-locally
   exits."
  (unwind-protect
       (handler-bind ((progress-condition handle-condition))
         (funcall thunk))
    (funcall update)))

(defun invoke-with-timed-update (interval update handle-condition thunk)
  ;; We request the timer function to be run in a fresh thread to
  ;; avoid interrupt/recursive locking problems: interrupting a thread
  ;; which may itself call handle or update would lead to either a
  ;; recursive lock attempt (with non-recursive locks) or "concurrent"
  ;; mutation of the info object (with recursive locks). This is, of
  ;; course, expensive, but I don't know another safe way.
  (let ((timer (sb-ext:make-timer update :name "Progress update" :thread t)))
    (unwind-protect
         (progn
           (sb-ext:schedule-timer timer interval :repeat-interval interval)
           (invoke-with-handler update handle-condition thunk))
      (sb-ext:unschedule-timer timer))))

(defmacro with-progress-report ((target
                                 &key
                                 style
                                 update
                                 (info-var (gensym "INFO")))
                                &body body)
  "Execute BODY, reporting progress conditions to TARGET using STYLE.

   UPDATE controls TODO

   Example

     (with-progress-report (*standard-output* :style :vertical)
       (with-sequence-progress (:processing work)
         (dolist (item work)
            (process item)
            (more-conditions:progress \"~A\" item))))"
  (once-only (target style update)
    `(let+ ((,info-var (make-instance 'global-progress-info))
            ((&flet update ()
               (report ,info-var ,target ,style)))
            ((&flet handle-condition (condition
                                      &optional
                                      (update? (eq ,update t)))
               (handle ,info-var condition)
               (when update? (update))))
            (thunk (lambda () ,@body)))
       (etypecase ,update
         ((or null (eql t))
          (invoke-with-handler #'update #'handle-condition thunk))
         (non-negative-real
          (invoke-with-timed-update
           ,update #'update #'handle-condition thunk))))))
