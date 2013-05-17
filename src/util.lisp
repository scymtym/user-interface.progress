;;;; util.lisp --- Utilities used by the user-interface.progress system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:user-interface.progress)

(defun now/seconds ()
  "Return time in seconds since an arbitrary point in time determined
   by `get-internal-real-time'."
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun draw-progress-bar (stream width progress &optional message)
  "Draw a bar indicating PROGRESS within WIDTH characters onto STREAM.

   If supplied, MESSAGE is embedded in the drawn progress bar."
  (declare (type progress-designator progress))
  (when (zerop width)
    (return-from draw-progress-bar))
  (let* ((index          (floor (progress->real progress) (/ width)))
         (message-length (when message (length message)))
         (text-left      (when message
                           (subseq message 0 (min index message-length))))
         (text-right     (when (and message (< index message-length))
                           (subseq message index (min width message-length))))) ; TODO ellipsis
    (terminal.ansi-escapes:format/markup
     stream
     "«bg: green, fg: white|~V,,,VA»«bg: white, fg: black|~V,,,VA»"
     index           #\Space (or text-left "")
     (- width index) #\Space (or text-right ""))))
