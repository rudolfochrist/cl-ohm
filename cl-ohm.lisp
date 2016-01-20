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

(defmacro with-connection (connection-plist &body body)
  `(redis:with-persistent-connection ,(if (null connection-plist)
                                          *redis-default-connection*
                                          connection-plist)
     ,@body))

(defmacro with-transactional-connection (connection-plist &body body)
  `(with-connection ,connection-plist
     (red:multi)
     ,@body
     (red:exec)))

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
  (:report (lambda (condition stream)
             (format stream "The object ~A is missing an ID and is therefore unmanaged. Please create~
managed objects with the function CREATE." (unmanaged-object condition)))))

(define-condition ohm-unmanged-class-error (ohm-error)
  ((class :initarg :class
          :accessor class))
  (:report (lambda (condition stream)
             (format stream "The class ~A is not a persistence class. Please use a class defined by DEFOHM."
                     (class condition)))))

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

(defun make-key (&rest segments)
  (with-output-to-string (out)
    (format out "~{~A~^:~}" segments)))

(defgeneric object-key (model &rest segments)
  (:documentation "Generates an Ohm model object key.")
  (:method :before ((model ohm-model) &rest segments)
           (declare (ignore segments))
           (unless (managed-object-p model)
             (error 'ohm-missig-id-error :object model)))
  (:method ((model ohm-model) &rest segments)
    (apply #'make-key (class-name (class-of model)) (id model) segments)))

(defgeneric class-key (model &rest segments)
  (:documentation "Generates a Ohm Model Class Key.")
  (:method :before ((model ohm-model) &rest segments)
           (declare (ignore segments))
           (unless (managed-object-p model)
             (error 'ohm-missig-id-error :object model)))
  (:method ((model ohm-model) &rest segments)
    (apply #'make-key (class-name (class-of model)) segments)))

(defun create (model-class &rest initargs)
  "Creates a new managed object."
  (assert (subtypep model-class 'ohm-model)
          (model-class)
          "Cannot create a persisted instance of ~A. Please use DEFOHM to define a suitable persistence type."
          model-class)
  (let ((instance (apply #'make-instance model-class initargs)))
    (with-connection ()
      (let ((id (red:incr +global-id-counter+)))
        (setf (slot-value instance 'id) id)
        instance))))

(defun normalize-tuples (tuples)
  "Normalizes TUPLES to be acceptable by MAKE-INSTANCE."
  (loop for (key value) on tuples by #'cddr
     nconc (list (make-keyword key) value)))

(defun object->plist (object)
  (loop for slot in (closer-mop:class-direct-slots (class-of object))
     nconc (let ((slot-name (closer-mop:slot-definition-name slot)))
             (when (slot-boundp object slot-name)
               (list slot-name (slot-value object slot-name))))))

(defun plist->object (class plist)
  (apply #'make-instance class plist))

(defmacro make-persisted-instance (class id initargs)
  "Creates a instance ARGS. ARGS is a Redis result set.
Obviously this is only sensible inside a WITH-CONNECTION block."
  (let ((ginstance (gensym "instance")))
    `(let ((,ginstance (plist->object ,class (normalize-tuples ,initargs))))
       (setf (slot-value ,ginstance 'id) ,id)
       ,ginstance)))

(defgeneric save (model)
  (:documentation "Saves the MODEL into the data store. Model must be a managed object defined with DEFOHM and created
with CREATE.")
  (:method :before ((model ohm-model))
           (unless (managed-object-p model)
             (error 'ohm-missig-id-error :object model)))
  (:method ((model ohm-model))
    (with-transactional-connection ()
      (apply #'red:hmset (object-key model) (object->plist model))
      (red:sadd (class-key model 'all) (id model)))))

(defgeneric del (model)
  (:documentation "Removes MODEL from the data store.")
  (:method :before ((model ohm-model))
           (unless (managed-object-p model)
             (error 'ohm-missig-id-error :object model)))
  (:method ((model ohm-model))
    (with-transactional-connection ()
      (red:del (object-key model))
      (red:srem (class-key model 'all) (id model)))))

(defmacro retrieve (class forms &key connection-plist)
  (let ((gclass (gensym "class"))
        (gid (gensym "id"))
        (gids (gensym "ids")))
    `(let ((,gclass ,class))
       (unless (subtypep ,gclass 'ohm-model)
         (error 'ohm-unmanged-class-error :class ,gclass))
       ,(cond
         ((eql forms :all)
          `(with-connection ,connection-plist
             (let ((,gids (red:smembers (make-key ,gclass 'all))))
               (mapcar (lambda (,gid)
                         (make-persisted-instance ,gclass
                                                  ,gid
                                                  (red:hgetall (make-key ,gclass ,gid))))
                       ,gids))))))))
