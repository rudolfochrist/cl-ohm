;;; mop.lisp

(in-package #:cl-ohm)

(defclass ohm-class (closer-mop:standard-class) ())

(defmethod closer-mop:validate-superclass ((class ohm-class) (superclass closer-mop:standard-class))
  t)

(defclass ohm-slot-definition-mixin ()
  ((indexp :initform nil
           :initarg :indexp
           :accessor indexp
           :documentation "Creates an index for this slot if indexp is T.")))

(defclass ohm-direct-slot-definition (closer-mop:standard-direct-slot-definition
                                      ohm-slot-definition-mixin)
  ())

(defmethod closer-mop:direct-slot-definition-class ((class ohm-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'ohm-direct-slot-definition))

(defclass ohm-effective-slot-definition (closer-mop:standard-effective-slot-definition
                                         ohm-slot-definition-mixin)
  ())

(defmethod closer-mop:effective-slot-definition-class ((class ohm-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'ohm-effective-slot-definition))

(defmethod closer-mop:compute-effective-slot-definition
    ((class ohm-class) name dslotds)
  (declare (ignore name))
  (let ((eslotd (call-next-method)))
    (loop for dslotd in dslotds
       when (typep dslotd 'ohm-direct-slot-definition)
       do (setf (indexp eslotd) (indexp dslotd)))
    eslotd))
