;;; mop.lisp

(in-package #:cl-ohm)

(defclass ohm-class (closer-mop:standard-class) ())

(defmethod closer-mop:validate-superclass ((class ohm-class) (superclass closer-mop:standard-class))
  t)

(defclass ohm-slot-definition-mixin ()
  ((indexp :initform nil
           :initarg :indexp
           :accessor indexp
           :documentation "Creates an index for this slot if indexp is T.")
   (counterp :initform nil
             :initarg :counterp
             :accessor counterp
             :documentation "Defines an attribute as counter.")
   (list-attr-p :initform nil
                :initarg :list-attr-p
                :accessor list-attr-p
                :documentation "Defines an attribute as list.")
   (set-attr-p :initform nil
               :initarg :set-attr-p
               :accessor set-attr-p
               :documentation "Defines an attribute as set.")
   (element-type :initform nil
                 :initarg :element-type
                 :accessor element-type
                 :documentation "Specifies the element type for lists or sets.")))

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
       do (setf (indexp eslotd) (indexp dslotd)
                (counterp eslotd) (counterp dslotd)
                (list-attr-p eslotd) (list-attr-p dslotd)
                (set-attr-p eslotd) (set-attr-p dslotd)
                (element-type eslotd) (element-type dslotd)))
    eslotd))
