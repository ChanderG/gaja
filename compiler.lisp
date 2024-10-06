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

;; only called on variable use
(defmethod ck-ident ((fco func-co) exp)
  (let* ((pos (position (cadr exp) (vars fco))))
    ;; if pos is nil, it means that a variable has been used without defining
    (assert pos)
    (emit fco 'op-load-name pos)))

(defmethod ck-arith-expr ((fco func-co) exp)
  (let* ((children (cdr exp))
         (binop (second (second children)))
         (binop-num (cdr (assoc binop arg-binop-sym-map))))
    (handle-expr fco (first children))
    (handle-expr fco (third children))
    (emit fco 'op-binary-op binop-num)))

(defmethod ck-print-stmt ((fco func-co) exp)
  (handle-expr fco (second exp))
  (emit fco 'op-print))

(defmethod ck-assign-stmt ((fco func-co) exp)
  (let* ((varname (second (second exp)))
         (pos (position varname (vars fco)))
         (num-vars (length (vars fco))))
    (handle-expr fco (third exp))
    (if pos
        ;; variable already exists in our defn
        (emit fco 'op-store-name pos)
        ;; variable does not exist, need to create it
        (progn
          (setf (vars fco) (append (vars fco) (list varname)))
          (emit fco 'op-store-name num-vars)))))

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

(defun hasti ()
  (let* ((input (second *posix-argv*))
         (output (third *posix-argv*))
         (output (if (not output) "out.gaja")))
    (if (not input)
        (progn
            (format t "Usage: hasti <input> [<output>]~%")
            (format t "No input passed, quitting.~%")
            (quit)))
    (format t "Compiling ~A to ~A.~%" input output)
    (serialize-co-to-file (compile-kapi-file input) output)))
