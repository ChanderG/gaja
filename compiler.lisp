(setq kapi1 "./parser/out.kapi")

(defun symbol-append (&rest symbols)
  (intern (apply #'concatenate 'string
                 (mapcar #'symbol-name symbols))))

(defun handle-expr (fco exp)
  (funcall (symbol-append 'ck- (car exp)) fco exp))

(defmethod emit ((fco func-co) &rest opcode)
  (setf (instr fco) (append (instr fco) (list opcode))))

(defmethod ck-numeral ((fco func-co) exp)
  ; save number into consts
  ; no check for repetition
  (let* ((curr-consts (consts fco))
         (len (length curr-consts)))
    (setf (consts fco) (append curr-consts (cdr exp)))
    (emit fco 'op-load-value len)))

(defmethod ck-arith-expr ((fco func-co) exp)
  (dolist (arg (cdr exp))
    (handle-expr fco arg))
  (emit fco 'op-add))

(defun ck-func (func)
  (let* ((name (cadr (nth 1 func)))
         (bl (nth 2 func))
         (fco (make-func-co name '() '() '())))
    (assert (eq (car bl) 'block))
    (dolist (exp (cdr bl))
      (handle-expr fco exp))
    fco))

(defun compile-kapi (exp &aux (fcos '()))
  (assert (eq (car exp) 'module))
  (dolist (func (cdr exp) (car fcos))
    (assert (eq (car func) 'func-defn))
    (assert (eq (car (nth 1 func)) 'ident))
    (format t "Processing function: ~A~%" (cadr (nth 1 func)))
    (push (ck-func func) fcos)))

(defun compile-kapi-file (filename)
  (let* ((exp (with-open-file (stream filename)
                (read stream))))
    (compile-kapi exp)))

(compile-kapi-file kapi1)
