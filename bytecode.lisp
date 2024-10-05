(defclass func-co ()
  ((name :initarg :name :accessor name)
   (instr :initarg :instr :accessor instr)
   (consts :initarg :consts :accessor consts)
   (vars :initarg :vars :accessor vars)))

(defun make-func-co (name instr consts vars)
  (make-instance 'func-co :name name :instr instr :consts consts :vars vars))

(defmethod print-object ((fco func-co) out)
  (print-unreadable-object (fco out :type t)
    (format out "~s~%" (name fco))
    (format out "~s~%" (instr fco))
    (format out "~s~%" (consts fco))
    (format out "~s" (vars fco))))

; example function code
(setq ex1
  (make-func-co "add"
    '((op-load-value 0)
     (op-load-value 1)
     (op-add)
     (op-print))
    '(7 5)
    '()))

(setq ex2
      (make-func-co "add-vars"
                    '((op-load-value 0)
                      (op-store-name 0)
                      (op-load-value 1)
                      (op-store-name 1)
                      (op-load-name 0)
                      (op-load-name 1)
                      (op-add)
                      (op-print))
                    '(1 2)
                    '(a b)))
