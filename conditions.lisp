;;; conditions.lisp

(in-package #:CL-OHM)

(define-condition ohm-error (error) ())

(define-condition ohm-missing-id-error (ohm-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Cannot save object wihtout ID. Please create one with CREATE."))))


(define-condition ohm-no-index-found-error (ohm-error)
  ((attribute :initarg :attribute
              :reader attribute))
  (:report (lambda (condition stream)
             (format stream "No index found for attribute ~A" (attribute condition)))))

(define-condition ohm-unique-constraint-violation (ohm-error)
  ((value :initarg :value
          :reader value))
  (:report (lambda (condition stream)
             (format stream "~A must be unique." (value condition)))))
