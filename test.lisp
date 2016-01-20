
(in-package :cl-ohm)

(defohm person ()
  ((first-name :initarg :first-name
               :accessor first-name
               :indexp t)
   (last-name :initarg :last-name
              :accessor last-name
              :indexp t)))

(defmacro with-unbound-slots (slots instance &body body)
  `(let ,(mapcar (lambda (slot-name)
                   (list slot-name `(when (slot-boundp ,instance ',slot-name)
                                      (slot-value ,instance ',slot-name))))
                 slots)
     ,@body))

(defmethod print-object ((object person) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-unbound-slots (id first-name last-name) object
      (format stream "(ID: ~A) (FIRST-NAME: ~A) (LAST-NAME: ~A)"
              id first-name last-name))))

(defmethod slot-unbound (class (instance person) slot-name)
  (declare (ignore class))
  (setf (slot-value instance slot-name) nil))


(defparameter *p* (create 'person :first-name "Otto" :last-name "Dix"))
