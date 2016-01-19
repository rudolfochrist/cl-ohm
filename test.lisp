
(in-package :cl-ohm)

(defohm person ()
  ((first-name :initarg :first-name
               :accessor first-name
               :index t)
   (last-name :initarg :last-name
              :accessor last-name)))

(defmethod full-name ((person person))
  (format nil "~a ~a" (first-name person) (last-name person)))

(defparameter *p* (create 'person :first-name "Otto" :last-name "Dix"))
