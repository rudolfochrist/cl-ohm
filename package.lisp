;;;; package.lisp

(defpackage #:cl-ohm
  (:nicknames #:ohm)
  (:use #:cl)
  (:import-from #:redis
                #:with-pipelining)
  (:import-from #:alexandria
                #:define-constant
                #:make-keyword))
