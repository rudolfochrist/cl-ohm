;;; cl-ohm.lisp

(in-package #:cl-ohm)

(defvar *global-object-counter*
  'cl-ohm-global-object-counter)

(defun create (name &rest initargs)
  "Creates a persisted instance of NAME."
  (assert (subtypep name 'ohm-object)
          (name)
          "~A is not persistable. Please use DEFINE-OHM-MODEL."
          name)
  (let ((instance (apply #'make-instance name initargs)))
    (with-connection ()
      (setf (slot-value instance 'id)
            (red:incr *global-object-counter*)))
    instance))

(defun ensure-peristable-object (object)
  (unless (slot-boundp object 'id)
    (error 'ohm-missing-id-error)))

(defun make-key (&rest segments)
  (format nil "~{~A~^:~}" segments))

(defgeneric class-key (object &rest segments)
  (:documentation "Creates a persistence key for class of OBJECT.")
  (:method :before ((object ohm-object) &rest segments)
           (declare (ignore segments))
           (ensure-peristable-object object))
  (:method ((object ohm-object) &rest segments)
    (apply #'make-key (class-name (class-of object)) segments)))

(defgeneric object-key (object &rest segments)
  (:documentation "Creates a persistence key for OBJECT.")
  (:method :before ((object ohm-object) &rest segments)
           (declare (ignore segments))
           (ensure-peristable-object object))
  (:method ((object ohm-object) &rest segments)
    (apply #'make-key
           (class-key object)
           (ohm-id object)
           segments)))

(defun create-indices (object)
  "Create indices for attributes with :indexp t."
  (loop for slot in (closer-mop:class-slots (class-of object))
     for slot-name = (closer-mop:slot-definition-name slot)
     when (and (slot-boundp object slot-name)
               (indexp slot))
     do
       (let ((index-key (class-key object
                                   'indices
                                   slot-name
                                   (slot-value object slot-name))))
         (red:sadd index-key (ohm-id object))
         (red:sadd (object-key object 'indices) index-key))))

(defgeneric save (object)
  (:documentation "Saves an object into the data store.")
  (:method :before ((object ohm-object))
           (ensure-peristable-object object))
  (:method ((object ohm-object))
    (let ((plist (object->plist object)))
      (with-connection ()
        (with-transaction ()
          (apply #'red:hmset (object-key object) plist)
          (red:sadd (class-key object 'all) (ohm-id object))
          (create-indices object))))))
