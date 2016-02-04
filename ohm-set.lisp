;;; ohm-set.lisp

(in-package #:cl-ohm)

(defclass ohm-set ()
  ;; TODO: those slots should be refactored to it's own
  ;; mixin, since it's basically the same everywhere.
  ((ket :reader set-key
        :initarg :key)
   (element-type :reader element-type
                 :initarg element-type)))
