(defclass vm ()
  ((stack :initform '() :accessor stack)
   (env :initform (make-hash-table :test #'equal) :accessor env)))

(defmethod run ((vm vm) (fun func-co))
  (format t "Running func: ~A~%" (name fun))
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

(defop op-store-name
  (setf (gethash arg (env vm)) (st-pop)))

(defop op-load-name
  (st-push (gethash arg (env vm))))

;; Entrypoint
(defun gaja ()
  (let* ((input (second *posix-argv*))
         (vm (make-instance 'vm)))
    (if (not input)
        (progn
          (format t "gaja vm: runs gaja bytecode files~%")
          (format t "Usage: gaja <input>.gaja~%")
          (format t "No input passed, quitting.~%")
          (quit)))
    (run vm (deserialize-co-from-file input))))
