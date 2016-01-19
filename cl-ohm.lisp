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

(define-condition ohm-missig-id-error (ohm-error)
  ((unmanaged-object :initarg :object
                     :accessor unmanaged-object))
  (:report (lambda (condtion stream))
           (format stream "The object ~A is missing a ID and is therefore unmanaged. Please create~
managed objects with the function CREATE." (unmanaged-object condition))))

(define-constant +global-id-counter+ "CL-OHM-GLOBAL-ID-COUNTER" :test #'string=)

(defclass ohm-model ()
  ((id :reader id)))

(defmacro defohm (name superclasses slots)
  `(defclass ,name ,(cons 'ohm-model superclasses)
     ,slots
     (:metaclass ohm-metaclass)))

(defgeneric managed-object-p (model)
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

(defun create (model-class &rest initargs)
  "Creates a new managed object."
  (assert (subtypep model-class 'ohm-model) nil
          "Cannot created an instance of ~A. Please use DEFOHM to define a suitable persistence type" model-class)
  (let ((instance (apply #'make-instance model-class initargs)))
    (with-connection ()
      (let ((id (red:incr +global-id-counter+)))
        (setf (slot-value instance 'id) id)
        instance))))
