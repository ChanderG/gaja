;;; Code objects

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

;;; Serialization formats

(defmacro deftag (name val)
  `(defconstant ,name ,val))

(defmacro defopcode0 (name val)
  `(defconstant ,name ,val))
(defmacro defopcode1 (name val)
  `(defconstant ,name ,val))

;; tags from 0-9 reserved for special use later

;; 10-40 for fundamental types
; short string of length <255, so that the length is represented in a single byte
(deftag t/short-string 10)
; next byte refers directly to a single byte char, no length
(deftag t/char 11)
; next byte refers to a an int <255; no length
(deftag t/small-uint 12)
; next byte refers to a a negative int <255; no length
(deftag t/small-nint 13)

;; 40 onwards for custom types
(deftag t/fco-start 40)
(deftag t/fco-end 41)
; denotes end of instruction range
; this means this cannot be an op
(deftag t/fco-instr-end 42)

;; Definition of opcodes
;; start from 127, just to ensure difference from the tags
;; strictly not necessary - as long as some special tag values don't clash with opcodes
(defopcode0 op/print 127)
(defopcode0 op/add 128)
(defopcode1 op/load-value 129)
(defopcode1 op/load-name 130)
(defopcode1 op/store-name 131)

(with-open-file (stream "sample.out"
                        :direction :output
                        :if-exists :overwrite
                        :element-type '(unsigned-byte 8))
  ;; our special magic string
  (write-sequence '(#x0b #x89 #x09 #x50) stream)
  (write-sequence (mapcar 'char-code '(#\g #\a #\j #\a)) stream)

  ;; start writing a function code obj
  (write-byte t/fco-start stream)

  ;; storing the function name
  (let* ((name (name ex1)))
    (write-byte t/short-string stream)
    (write-byte 4 stream)
    (write-sequence (map 'list 'char-code name) stream)
    )

  ;; followed by a list of instructions
  (dolist (ins (instr ex1))
    (let* ((opname (car ins))
           (opname-str (symbol-name opname))
           (new-str (substitute #\/ #\- opname-str :count 1))
           (op-sym (eval (read-from-string new-str))))
      ;; write out the opcode itself
      (write-byte op-sym stream)
      ;; write down the argument here, if present
      ;; for now - assumed to be one byte only
      (if (eq (length ins) 2)
        (write-byte (cadr ins) stream))))

  (write-byte t/fco-instr-end stream)

  ;; end func co obj
  (write-byte t/fco-end stream)
  )

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
