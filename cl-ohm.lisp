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

(defmacro with-connection (() &body body)
  `(redis:with-persistent-connection ,*redis-default-connection*
     ,@body))

(defmacro with-transactional-connection (()  &body body)
  `(with-connection ()
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
  ((unmanaged-class :initarg :class
                    :accessor unmanaged-class))
  (:report (lambda (condition stream)
             (format stream "The class ~A is not a persistence class. Please use a class defined by DEFOHM."
                     (unmanaged-class condition)))))

(define-constant +global-id-counter+ "CL-OHM-GLOBAL-ID-COUNTER" :test #'string=)

(defclass ohm-model ()
  ((id :reader id)
   (savedp :reader savedp)))

(defmacro defohm (name superclasses slots)
  `(defclass ,name ,(if superclasses superclasses `(ohm-model))
     ,slots
     (:metaclass ohm-metaclass)))

(defun find-slot (class slot-name)
  (find slot-name (closer-mop:class-direct-slots class) :key #'closer-mop:slot-definition-name))

(defun ohm-model-slot-p (slot-name)
  "Checks if slot-name is defined in OHM-MODEL."
  (find-slot (find-class 'ohm-model) slot-name))

(defgeneric managed-object-p (model)
  (:documentation "Tests if the given MODEL has been persisted resp. is dirty or not.")
  (:method ((model ohm-model))
    (and (slot-boundp model 'id)
         (not (null (id model))))))

(defun make-key (&rest segments)
  (with-output-to-string (out)
    (format out "~{~A~^:~}" segments)))

(defun ensure-managed-object (object)
  (unless (managed-object-p object)
    (error 'ohm-missig-id-error :object object)))

(defgeneric object-key (model &rest segments)
  (:documentation "Generates an Ohm model object key.")
  (:method :before ((model ohm-model) &rest segments)
           (declare (ignore segments))
           (ensure-managed-object model))
  (:method ((model ohm-model) &rest segments)
    (apply #'make-key (class-name (class-of model)) (id model) segments)))

(defgeneric class-key (model &rest segments)
  (:documentation "Generates a Ohm Model Class Key.")
  (:method :before ((model ohm-model) &rest segments)
           (declare (ignore segments))
           (ensure-managed-object model))
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

(defun create! (model-class &rest initargs)
  "Creates a new managed object and saves it to the data store immediately."
  (save (apply #'create model-class initargs)))

(defun normalize-tuples (tuples)
  "Normalizes TUPLES to be acceptable by MAKE-INSTANCE."
  (loop for (key value) on tuples by #'cddr
     nconc (list (make-keyword key) value)))

(defun object->plist (object)
  "Takes an OBJECT and represents it a plist.
This function returns two lists. A object attributes plist and
a second list with index keys."
  (loop with index-keys = nil
     for slot in (closer-mop:class-slots (class-of object))
     for slot-name = (closer-mop:slot-definition-name slot)
     when (slot-boundp object slot-name)
     unless (ohm-model-slot-p slot-name)
     nconc (let ((slot-value (slot-value object slot-name)))
             (when (indexp slot)
               (push (class-key object 'indices slot-name slot-value) index-keys))
             (list (make-keyword slot-name)
                   (slot-value object slot-name)))
     into attributes-plist
     finally (return (values attributes-plist index-keys))))

(defun plist->object (class plist)
  (apply #'make-instance class plist))

(defun make-persisted-instance (class id &optional initargs)
  "Creates a instance ARGS. ARGS is a Redis result set.
Obviously this is only sensible inside a WITH-CONNECTION block."
  (let ((instance (plist->object class (normalize-tuples initargs))))
    (setf (slot-value instance 'id) id)
    (setf (slot-value instance 'savedp) t)
    instance))

(defun make-persisted-instances (class ids)
  (loop for id in ids
     collect (make-persisted-instance class id (fetch-data class id))))

(defun fetch-data (class id)
  "Fetches an object's attributes from the data store.
Signals an error when non-existing ID is given."
  (red:hgetall (make-key class id)))

(defgeneric save (model)
  (:documentation "Saves the MODEL into the data store. Model must be a managed object defined with DEFOHM and created
with CREATE.")
  (:method :before ((model ohm-model))
           (ensure-managed-object model))
  (:method ((model ohm-model))
    (with-transactional-connection ()
      (multiple-value-bind (attributes index-keys)
          (object->plist model)
        (apply #'red:hmset (object-key model) attributes)
        (apply #'red:sadd (object-key model '_indices) index-keys)
        (dolist (index index-keys)
          (red:sadd index (id model)))
        (red:sadd (class-key model 'all) (id model))
        (setf (slot-value model 'savedp) t)))))

(defgeneric del (model)
  (:documentation "Removes MODEL from the data store.")
  (:method :before ((model ohm-model))
           (ensure-managed-object model))
  (:method ((model ohm-model))
    (with-connection ()
      (let* ((indices-key (object-key model '_indices))
             (indices (red:smembers indices-key)))
        (red:multi)
        (dolist (index indices)
          (red:srem index (id model)))
        (red:srem (class-key model 'all) (id model))
        (red:del indices-key (object-key model))
        (red:exec)))))

(defmacro retrieve (class &rest params)
  (let ((gclass (gensym "class"))
        (gid (gensym "id"))
        (gids (gensym "ids"))
        (gkey (gensym "key"))
        (gvalue (gensym "value")))
    `(let ((,gclass ,class))
       (unless (subtypep ,gclass 'ohm-model)
         (error 'ohm-unmanged-class-error :class ,gclass))
       (with-connection ()
         ,(cond
           ((eql (car params) :all)
            `(let ((,gids (red:smembers (make-key ,gclass 'all))))
               (make-persisted-instances ,gclass ,gids)))
           ((eql (car params) :id)
            `(let ((,gid ,(second params)))
               (when (red:exists (make-key ,gclass ,gid))
                 (make-persisted-instance ,gclass ,gid (fetch-data ,gclass ,gid)))))
           (t
            `(let ((,gids (apply #'red:sinter
                                 (loop for (,gkey ,gvalue) on ',params by #'cddr
                                    collect (make-key ,gclass 'indices ,gkey ,gvalue)))))
               (make-persisted-instances ,gclass ,gids))))))))

(defgeneric counter (model slot-name)
  (:documentation "Returns the value of the counter named SLOT-NAME.")
  (:method :before ((model ohm-model) slot-name)
           (declare (ignore slot-name))
           (ensure-managed-object model))
  (:method ((model ohm-model) slot-name)
    (let ((slot (find-slot (class-of model) slot-name)))
      (when (counterp slot)
        ;; access redis for counter
        ))))

(defgeneric incr (model slot-name &optional value)
  (:documentation "Increments the counter named SLOT-NAME by VALUE.")
  (:method :before ((model ohm-model) slot-name &optional value)
           (declare (ignore slot-name value))
           (ensure-managed-object model))
  (:method ((model ohm-model) slot-name &optional (value 1))
    (let ((slot (find-slot (class-of model) slot-name)))
      (when (counterp slot)
        ;; increment
        ))))

(defgeneric decr (model slot-name &optional value)
  (:documentation "Decrements the counter named SLOT-NAME by VALUE.")
  (:method :before ((model ohm-model) slot-name &optional value)
           (declare (ignore slot-name value))
           (ensure-managed-object model))
  (:method ((model ohm-model) slot-name &optional (value 1))
    (let ((slot (find-slot (class-of model) slot-name)))
      (when (counterp slot)
        ;; decrement
        ))))
