
(in-package :cl-ohm)

(defohm person ()
  ((first-name :initarg :first-name
               :accessor first-name
               :index t)
   (last-name :initarg :last-name
              :accessor last-name)))

(defmethod print-object ((object person) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "[ID: ~A] [FIRST-NAME: ~A] [LAST-NAME: ~A]"
            (if (slot-boundp object 'id)
                (id object)
                "NO-ID")
            (first-name object)
            (last-name object))))

(defmethod full-name ((person person))
  (format nil "~a ~a" (first-name person) (last-name person)))

(defparameter *p* (create 'person :first-name "Otto" :last-name "Dix"))
