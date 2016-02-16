;;;; cl-ohm.asd

(in-package :asdf-user)

(defsystem #:cl-ohm
  :version "0.1.0"
  :description "An object-hash mapping for Redis in Common Lisp"
  :author "Sebastian Christ <rudolfo.christ@gmail.com>"
  :mailto "rudolfo.christ@gmail.com"
  :homepage "https://github.com/rudolfochrist/cl-ohm"
  :source-control (:git "git@github.com:rudolfochrist/cl-ohm.git")
  :bug-tracker "https://github.com/rudolfochrist/cl-ohm/issues"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "connection")
               (:file "conditions")
               (:file "mop")
               (:file "ohm-object")
               (:file "ohm-counter")
               (:file "ohm-collection")
               (:file "ohm-list")
               (:file "ohm-set-algebra")
               (:file "ohm-set")
               (:file "cl-ohm"))
  :depends-on (#:alexandria
               #:closer-mop
               #:cl-redis))
