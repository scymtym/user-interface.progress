;;;; report-mixins.lisp --- Mixins for progress reporting styles.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:user-interface.progress)

;;; `overwrite-managing-mixin'

(defclass overwrite-managing-mixin ()
  ((previous-num-lines :initarg  :previous-num-lines
                       :type     (or null non-negative-integer)
                       :accessor style-previous-num-lines
                       :initform nil
                       :documentation
                       "Stores the amount of lines written to the
                        target stream by the previous `report' call so
                        these lines can be erased before new output is
                        written."))
  (:documentation
   "This class is intended to be mixed into report classes which write
    and then update, by overwriting the text region, a report to a
    text terminal."))

(defmethod report :around ((info   global-progress-info)
                           (target stream)
                           (style  overwrite-managing-mixin)
                           &key)
  (let+ (((&accessors (lines style-previous-num-lines)) style))
    ;; If we have written lines in previous calls, erase that number
    ;; of lines before producing output.
    (when lines
      (iter (repeat lines)
            (write-string #.(format nil "~C[1F~:*~C[2K" #\Escape) target)))
    ;; Produce output and record the number of produced lines.
    (setf lines (call-next-method))
    (finish-output target)
    (values)))

;;; `width-mixin'

(defclass width-mixin ()
  ((width :initarg  :width
          :type     positive-integer
          :accessor style-width
          :documentation
          "Stores the number of columns available in the target
           stream."))
  (:default-initargs
   :width (parse-integer (or #+sbcl (sb-posix:getenv "COLUMNS")
                             "70")))
  (:documentation
   "This class is intended to mixed into report classes which have to
    determine the number of columns in the target stream."))

(defmethod report :around ((info   global-progress-info)
                           (target t)
                           (style  width-mixin)
                           &rest args &key
                           (width (style-width style)))
  (apply #'call-next-method info target style
         :width width (remove-from-plist args :width)))
