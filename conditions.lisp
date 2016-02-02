;;; conditions.lisp

(in-package #:CL-OHM)

(define-condition ohm-error (error) ())

(define-condition ohm-missing-id-error (ohm-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Cannot save object wihtout ID. Please create one with CREATE."))))
