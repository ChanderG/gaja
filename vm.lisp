;; example of the function bytecode
; (name instr_list consts vars)

;; example of the program bytecode
; list of function's, with a special top level

;; Defining instruction encoding
; (setq *opcode_num* 0)

(defclass func-co ()
  ((name :initarg :name :accessor name)
   (instr :initarg :instr :accessor instr)
   (consts :initarg :consts :accessor consts)))

(defun make-func-co (name instr consts)
  (make-instance 'func-co :name name :instr instr :consts consts))

; example function code
(setq ex1
  (make-func-co "add"
    '((op-load-value 0)
     (op-load-value 1)
     (op-add)
     (op-print))
   '(7 5)))

(defclass vm ()
  ((stack :initform '() :accessor stack)
   (env :initform (make-hash-table :test #'equal) :accessor env)))

(setq *vm* (make-instance 'vm))

(defmethod run ((vm vm) (fun func-co))
  (format t "Running func: ~A" (name fun))
  (dolist (ins (instr fun))
    (let* ((op (car ins))
           (arg (cadr ins)))
      (funcall op vm fun arg))))

;; Helper macro to define VM ops
;; makes available the vm and the function to the body forms
(defmacro defop (name &body forms)
  `(defmethod ,name ((vm vm) (fun func-co) arg)
     ,@forms))

(defmacro st-push (val)
  `(push ,val (stack vm)))

(defmacro st-pop ()
  '(pop (stack vm)))

;; VM operations

(defop op-add
  (let* ((arg2 (st-pop))
         (arg1 (st-pop)))
    (st-push (+ arg1 arg2))))

(defop op-print
  (print (st-pop)))

(defop op-load-value
  (let ((val (nth arg (consts fun))))
    (st-push val)))
