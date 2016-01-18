;;;; cl-ohm.lisp

(in-package #:cl-ohm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *redis-default-connection*
    '(:host #(127 0 0 1) :port 6379 :auth nil)))

(defun setup (&key
                (host nil host-supplied-p)
                (port nil port-supllied-p)
                (auth nil auth-supplied-p))
  (when host-supplied-p
    (setf (getf *redis-default-connection* :host) host))
  (when port-supllied-p
    (setf (getf *redis-default-connection* :port) port))
  (when auth-supplied-p
    (setf (getf *redis-default-connection* :auth) auth)))

(defmacro with-connection (connection-details &body body)
  `(redis:with-persistent-connection ,(if (null connection-details)
                                          *redis-default-connection*
                                          connection-details)
     ,@body))

(define-condition ohm-error (error)
  ())

(define-condition ohm-connection-error (ohm-error)
  ((host :initarg :host
         :reader host)
   (port :initarg :port
         :reader port)
   (auth :initarg :auth
         :reader auth))
  (:report (lambda (condition stream)
             (format stream "Connection refused to Redis at ~A:~A with credentials ~A"
                     (host condition) (port condition) (auth condition)))))

(defclass ohm-model ()
  ((id :reader id)))

(defclass ohm-metaclass (standard-class)
  ())

(defmacro defohm (name superclasses slots)
  `(defclass ,name ,(cons 'ohm-model superclasses)
     ,slots
     (:metaclass ohm-metaclass)))

(defmethod closer-mop:validate-superclass ((class ohm-metaclass) (super standard-class))
  t)

(defgeneric persisted-p (model)
  (:documentation "Tests if the given MODEL has been persisted resp. is dirty or not.")
  (:method ((model ohm-model))
    (slot-boundp model 'id)))

(defgeneric key (model &rest segments)
  (:documentation "Ohm Model key accessor.")
  (:method ((model ohm-model) &rest segments)
    (when (persisted-p model)

      (with-output-to-string (out)
        (format out "~A:~A" (class-name (class-of model)) (id model))
        (when (consp segments)
          (format out "~{:~A~}" segments))))))
