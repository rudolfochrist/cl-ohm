;;; test/run-tests.lisp

(in-package #:cl-ohm)

(defun run-all-tests ()
  (let ((suites '(attributes counters lists sets)))
    (dolist (suite suites)
      (5am:run! suite))))
