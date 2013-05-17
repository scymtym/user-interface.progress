;;;; info.lisp --- Storage of progress information.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:user-interface.progress)

;;; `global-progress-info' Class
;;;
;;; Stores progress information for threads.

(defclass global-progress-info (timing-mixin)
  ((threads :type     hash-table
            :reader   info-%threads
            :initform (make-hash-table :test #'eq)
            :documentation
            "Stores a mapping of threads to `thread-progress-info'
             instances.")
   (lock    :reader    info-%lock
            :initform  (bt:make-lock)
            :documentation
            "Stores a lock which protects against concurrent mutation
             form multiple threads."))
  (:documentation
   "Instances of this class store global (i.e. across multiple
    threads) progress information."))

(defmethod info-progress ((info global-progress-info))
  (let ((children (info-children info)))
    (case (length children)
      (0 t)
      (1 (info-progress (first children)))
      (t (mean (mapcar (compose #'progress->real #'info-progress)
                       children))))))

(defmethod info-children ((info global-progress-info))
  (hash-table-values (info-%threads info)))

(defmethod handle :around ((info global-progress-info) (condition t))
  (bt:with-lock-held ((info-%lock info))
    (call-next-method)))

(defmethod handle ((info global-progress-info) (condition condition))
  ;; Find the thread progress info object for the current thread,
  ;; creating it, if necessary and let it handle CONDITION.
  (let* ((thread      (bt:current-thread))
         (thread-info (ensure-gethash thread (info-%threads info)
                                      (make-instance 'thread-progress-info))))
    (handle thread-info condition)
    ;; Handling CONDITION may cause the operation stack in THREAD-INFO
    ;; to become empty. Remove THREAD-INFO if that happens.
    (when (emptyp (info-operations thread-info))
      (remhash thread (info-%threads info)))))

(defmethod report :around ((info   global-progress-info)
                           (target t)
                           (style  t)
                           &key)
   (bt:with-lock-held ((info-%lock info))
     (call-next-method)))

;;; `thread-progress-info' class
;;;
;;; Stores progress information for a stack of operations in a thread.

(defclass thread-progress-info (timing-mixin)
  ((operations :initarg  :operations
               :type     list
               :accessor info-operations
               :reader   info-children
               :initform '()
               :documentation
               "Stores a stack of operations (as
                `operation-progress-info' instances) currently being
                performed by the thread where the first element
                corresponds to the innermost operation."))
  (:documentation
   "Instances of this class store and manage progress information for
    the stack of nested operations being performed within a particular
    thread."))

(defmethod info-progress ((info thread-progress-info))
  (when-let ((outer (lastcar (info-operations info))))
    (info-progress outer)))

(defmethod info-remove-finished ((info thread-progress-info))
  (let+ (((&accessors (operations info-operations)) info))
    (iter (while (and operations (info-finished? (first operations))))
          (pop operations))))

(defmethod handle :after ((info thread-progress-info) (condition condition))
  (info-remove-finished info))

(defmethod handle ((info thread-progress-info) (condition condition))
  (let+ (((&accessors (operations info-operations)) info)
         ((&accessors-r/o (operation progress-condition-operation)) condition)
         (current-operation (first operations)))
    ;; Ensure the top element of the operation stack corresponds to
    ;; OPERATION.
    (when (or (not current-operation)
              (not (eq (info-operation current-operation) operation)))
      (push (setf current-operation
                  (make-instance 'operation-progress-info
                                 :operation operation))
            operations))
    ;; Delegate handling of CONDITION to the `operation-progress-info'
    ;; for OPERATION.
    (handle current-operation condition)))

;;; `operation-progress-info' class
;;;
;;; Progress information for individual operations within threads.

(defclass operation-progress-info (timing-mixin)
  ((operation             :initarg :operation
                          :type    symbol
                          :reader  info-operation
                          :documentation
                          "Stores the name of the operation described
                           by the info object.")
   (most-recent-condition :type     (or null condition)
                          :accessor info-most-recent-condition
                          :initform nil
                          :documentation
                          "Stores the most recently handled
                           condition."))
  (:default-initargs
   :operation (missing-required-initarg
               'operation-progress-info :operation))
  (:documentation
   "Instances of this class track the respective progress of
    individual operations."))

(defmethod info-progress ((info operation-progress-info))
  (when-let ((condition (info-most-recent-condition info)))
    (progress-condition-progress condition)))

(defmethod handle ((info operation-progress-info) (condition condition))
  (setf (info-most-recent-condition info) condition))
