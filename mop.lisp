;;; mop.lisp

(in-package #:cl-ohm)

(defclass ohm-metaclass (closer-mop:standard-class)
  ())

(defmethod closer-mop:validate-superclass ((class ohm-metaclass) (super closer-mop:standard-class))
  t)

(defclass ohm-slot-definition-mixin ()
  ((indexp :accessor indexp
           :initarg :indexp
           :initform nil
           :documentation "Indicates if this attribute should be indexed. Defaults to NIL.")))

(defclass ohm-direct-slot-definition (closer-mop:standard-direct-slot-definition
                                      ohm-slot-definition-mixin)
  ())

(defmethod closer-mop:direct-slot-definition-class ((class ohm-metaclass) &rest initargs)
  (declare (ignore initargs))
  (find-class 'ohm-direct-slot-definition))

(defclass ohm-effective-slot-definition (closer-mop:standard-effective-slot-definition
                                         ohm-slot-definition-mixin)
  ())

(defmethod closer-mop:effective-slot-definition-class ((class ohm-metaclass) &rest initargs)
  (declare (ignore initargs))
  (find-class 'ohm-effective-slot-definition))

(defmethod closer-mop:compute-effective-slot-definition ((class ohm-metaclass) slot-name dslotds)
  (declare (ignore slot-name))
  (let ((esd (call-next-method)))
    (loop for dsd in dslotds
       when (typep dsd 'ohm-direct-slot-definition)
       do (setf (indexp esd) (indexp dsd))
       return dsd)
    esd))
