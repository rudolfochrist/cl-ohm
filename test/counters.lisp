;;; test/counters.lisp

(in-package #:cl-ohm)

(5am:def-suite counters)
(5am:in-suite counters)

(define-ohm-model candidate (person)
  :counters (votes))

(5am:test counting
  (flush-db)
  (let ((bill (create 'candidate :first-name "Bill")))
    (incr (votes bill))
    (incr (votes bill))

    (5am:is (= 2 (counter (votes bill))))

    (decr (votes bill) 2)

    (5am:is (zerop (counter (votes bill))))))
