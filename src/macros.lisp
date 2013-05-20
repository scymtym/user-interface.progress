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
  #-sbcl (error "~@<Don't know how to do timers in this Lisp.~@:>")
  ;; We request the timer function to be run in a fresh thread to
  ;; avoid interrupt/recursive locking problems: interrupting a thread
  ;; which may itself call handle or update would lead to either a
  ;; recursive lock attempt (with non-recursive locks) or "concurrent"
  ;; mutation of the info object (with recursive locks). This is, of
  ;; course, expensive, but I don't know another safe way.
  #+sbcl
  (let ((timer (sb-ext:make-timer update :name "Progress update" :thread t)))
    (unwind-protect
         (progn
           (sb-ext:schedule-timer timer interval :repeat-interval interval)
           (invoke-with-handler update handle-condition thunk))
      (sb-ext:unschedule-timer timer))))

(defmacro with-progress-report ((target
                                 &key
                                 (style      (missing-required-argument :style))
                                 update
                                 (info-var   (gensym "INFO"))
                                 (info-class ''global-progress-info))
                                &body body
                                &environment env)
  "Execute BODY, reporting progress conditions to TARGET using STYLE.

   Within the lexical scope of BODY, the function
   `user-interface.progress:update' can be called without any
   arguments to force a progress report to TARGET.

   STYLE has to be an object implementing the report
   protocol (actually, only the `report' generic function). The client
   has to take care to only supply TARGETs which STYLE can handle.

   UPDATE controls how and when progress reports to TARGET are
   produced. The following values are possible

   NIL

     Do not produce reports automatically, except after executing
     BODY. Reports during the execution of BODY have to be triggered
     by calling `update' (mentioned above.

   T

     Each progress condition signaled form BODY immediately causes a
     progress report to be written to TARGET. This mode should be used
     with case since it may slow down operations in BODY.

   POSITIVE-REAL

     Progress conditions signaled from BODY do not cause
     reports. Instead, every POSITIVE-REAL seconds, a report is
     written to TARGET (from a separate thread).

     Note: this may signal an error at compile-time or runtime in
     Lisps which do not support timers.

   Example

     (with-progress-report (*standard-output* :style :vertical)
       (with-sequence-progress (:processing work)
         (dolist (item work)
            (process item)
            (more-conditions:progress \"~A\" item))))"
  ;; If UPDATE is constant, do some compile-time sanity checks.
  (when (constantp update env)
    (let ((value (eval update)))
      (unless (typep value '(or (member nil t) positive-real))
        (warn "~@<The value of ~S is ~S which is not ~S, ~S or a ~
             ~S.~@:>"
              :update value nil t 'positive-real))
      #-sbcl (when (realp value)
               (warn "~@<~S ~S requested, but don't know how to do ~
                      timers in this Lisp.~@:>"
                     :update update))))

  (once-only (target style update)
    `(let+ ((,info-var (make-instance ,info-class))
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
         (positive-real
          (invoke-with-timed-update
           ,update #'update #'handle-condition thunk))))))
