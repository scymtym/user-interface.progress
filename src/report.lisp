;;;; report.lisp --- Progress reporting styles.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:user-interface.progress)

;;; "Summary" report style

(defclass summary (overwrite-managing-mixin
                   width-mixin)
  ()
  (:documentation
   "This report style prints a very simple summary onto a single line
    and then updates this summary as progress conditions are
    signaled."))

(defmethod report ((info   global-progress-info)
                   (target t)
                   (style  summary)
                   &key
                   width)
  (let+ (((&accessors-r/o (time     info-duration)
                          (progress info-progress)) info))
    (case progress
      ((t)
       0)
      (t
       (format target "~6,2F s ~
                       ~/more-conditions:print-progress-percentage/ "
               time progress)
       (draw-progress-bar target (- width 40) progress)
       (terpri target)
       ;; Produced one line of output.
       1))))

;;; "Vertical" report style

(defclass vertical (overwrite-managing-mixin
                    width-mixin)
  ()
  (:documentation
   "This report style print a and updates a detailed progress report
    over multiple lines. Information for each thread is printed in one
    \"block\" and within each such block details are printed for all
    operations on the thread's operation stack."))

(defmethod report ((info   global-progress-info)
                   (target t)
                   (style  vertical)
                   &key
                   width)
  (iter (for child in (info-children info))
        (unless (first-iteration-p)
          (format target "~A~%" (make-string width :initial-element #\―))
          (summing 1))
        (summing (report child target style :width width))))

(defmethod report ((info   thread-progress-info)
                   (target t)
                   (style  vertical)
                   &key
                   width)
  (flet ((do-condition (info level)
           (let+ (((&accessors-r/o (operation info-operation)
                                   (condition info-most-recent-condition)
                                   (time      info-duration)) info)
                  (progress        (progress->real (info-progress info)))
                  (message         (progress-condition-message condition))

                  (available-width (- width 1 8 1 8 1 10 1))
                  (operation-width (min (max (floor available-width 3) 12) 32))
                  (bar-width       (- available-width operation-width)))
             (terminal.ansi-escapes:format/markup
              target "~V@T«bold: t, fg: blue|~VA»"
              (* 2 level) (- operation-width (* 2 level)) operation)

             (when (plusp (- width operation-width 1 8 1 10))
               (format target " ~/more-conditions:print-progress-percentage/"
                       (progress-condition-progress condition)))

             (when (> bar-width 2)
               (write-char #\Space target)
               (draw-progress-bar target (1- bar-width) progress message))

             (terpri target))
           (1+ level)))
    ;; Return number of lines written
    (reduce #'do-condition (info-children info)
            :initial-value 0 :from-end t)))
