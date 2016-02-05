;;; ohm-set-algebra.lisp

(in-package #:cl-ohm)

(defvar *commands*
  '((red:sinter . red:sinterstore)
    (red:sunion . red:sunionstore)
    (red:sdiff . red:sdiffstore))
  "Command mappings.")

(defun command (expr)
  (cdr (assoc expr *commands*)))

(defun convert-expr (expr)
  (declare (special *ids* *ops*))
  (let* ((head (car expr))
         (tail (cdr expr))
         (id (format nil "stal:~D" (length *ids*)))
         (op (list (command head) id)))
    (push id *ids*)
    (setf op (append op
                     (compile-expr tail)))
    (push op *ops*)
    id))

(defun compile-expr (expr)
  (mapcar (lambda (item)
            (if (listp item)
                (convert-expr item)
                item))
          expr))

(defun explain (expr)
  (let ((*ops* '())
        (*ids* '()))
    (declare (special *ops* *ids*))
    (push (compile-expr expr)
          *ops*)
    (values (nreverse *ops*)
            *ids*)))

(defun execute (expr)
  (multiple-value-bind (ops ids)
      (explain expr)
    (with-connection ()
      (cond
        ((onep (length ops))
         (apply #'funcall (car ops)))
        (t
         (let ((results (with-pipelining
                          (with-transaction
                            (dolist (op ops)
                              (apply #'funcall op))
                            (apply #'red:del ids)))))
           (nth 1 (first (last results)))))))))
