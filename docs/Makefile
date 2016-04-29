# Create documentation

all: clean %.html

README.md: ../README.org
	pandoc -f org -t markdown -o $@ $<

%.html: README.md
	sbcl --no-sysinit --no-userinit --non-interactive \
	--eval "(setf sb-impl::*default-external-format* :UTF-8)" \
	--eval "(require 'asdf)" \
	--eval '(load "~/quicklisp/setup.lisp")' \
	--eval "(ql:quickload '(cl-ohm staple))" \
	--eval '(setf version (asdf:component-version (asdf:find-system :cl-ohm)))' \
	--eval '(setf docs-path (namestring (merge-pathnames "docs/" (asdf:system-source-directory :cl-ohm))))' \
	--eval "(staple:generate :cl-ohm :out (concatenate 'string docs-path \"cl-ohm-\" version \".html\"))"

.PHONY: clean
clean:
	-rm *.md *.html
