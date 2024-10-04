(setq kapi1 "./parser/out.kapi")

(defun symbol-append (&rest symbols)
  (intern (apply #'concatenate 'string
                 (mapcar #'symbol-name symbols))))

(defun handle-expr (exp)
  (funcall (symbol-append 'ck- (car exp)) exp))

(defun emit (opcode &optional arg)
  (format t "[~A]: ~A~%" opcode arg))

(defun ck-numeral (exp)
  (emit 'op-load-value 0))

(defun ck-arith-expr (exp)
  (dolist (arg (cdr exp))
    (handle-expr arg))
  (emit 'op-add))

(defun ck-func (func)
  (let* ((bl (nth 2 func)))
    (assert (eq (car bl) 'block))
    (dolist (exp (cdr bl))
      (handle-expr exp))))

(defun compile-kapi (exp)
  (assert (eq (car exp) 'module))
  (dolist (func (cdr exp))
    (assert (eq (car func) 'func-defn))
    (assert (eq (car (nth 1 func)) 'ident))
    (format t "Processing function: ~A~%" (cadr (nth 1 func)))
    (ck-func func)))

(defun compile-kapi-file (filename)
  (with-open-file (stream filename)
    (compile-kapi (read stream))))
