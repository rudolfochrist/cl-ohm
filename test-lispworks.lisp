;;; test-lispworks.lisp

;;; Script to test CL-OHM easier on LispWorks Personal Edition

(compile-file-if-needed #p "~/quicklisp/asdf.lisp" :load t)
(compile-file-if-needed #p "~/quicklisp/setup.lisp" :load t)

(ql:quickload '(cl-ohm cl-ohm/test))

(asdf:test-system :cl-ohm)
