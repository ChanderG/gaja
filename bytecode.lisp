;;; Code objects

(defclass func-co ()
  ((name :initarg :name :accessor name)
   (instr :initarg :instr :accessor instr :initform '())
   (consts :initarg :consts :accessor consts :initform '())
   (vars :initarg :vars :accessor vars :initform '())))

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

(defconstant opc0-start 127)
(defconstant opc1-start 190)
(defvar opc0-curr opc0-start)
(defvar opc1-curr opc1-start)

;; (setq opc0-curr opc0-start)
;; (setq opc1-curr opc1-start)

(defvar op-name-mapping (make-array 127))

;; forward mapping is through creation of constants
;; reverse mapping is an array of symbols

(defmacro defopcode0 (name)
  `(progn
     (defconstant ,name (incf opc0-curr))
     (setf (aref op-name-mapping (- opc0-curr 127))
        (quote ,name))))

(defmacro defopcode1 (name)
  `(progn
     (defconstant ,name (incf opc1-curr))
     (setf (aref op-name-mapping (- opc1-curr 127))
        (quote ,name))))

(defun is-opcode1 (val)
  (and (>= val opc1-start) (<= val opc1-curr)))

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
(defopcode0 op/print)
(defopcode0 op/add)

(defopcode1 op/load-value)
(defopcode1 op/load-name)
(defopcode1 op/store-name)

(defvar *magic* '())

;; hint: look at the unicode codepoints spelled out
(setf *magic* (append *magic* '(#x0b #x89 #x09 #x50)))
;; specific to our vm
(setf *magic* (append *magic* (mapcar 'char-code '(#\g #\a #\j #\a))))

(defun serialize-co-to-file (fco filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :overwrite
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    ;; our special magic string
    (write-sequence *magic* stream)

    ;; start writing a function code obj
    (write-byte t/fco-start stream)

    ;; storing the function name
    (let* ((name (name fco)))
      (write-byte t/short-string stream)
      (write-byte (length name) stream)
      (write-sequence (map 'list 'char-code name) stream)
      )

    ;; followed by a list of instructions
    (dolist (ins (instr fco))
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
    ))

(serialize-co-to-file ex1 "sample.gaja")

(defun deserialize-fco-from-stream (stream &aux
                                             (fco (make-instance 'func-co)))
  ;; name of a function
  (assert (eq (read-byte stream) t/short-string))
  (let* ((len (read-byte stream))
         (bytes (make-sequence 'list len)))
    (read-sequence bytes stream)
    (setf (name fco) (concatenate 'string (mapcar #'code-char bytes))))

  ;; instructions
  (loop
   (let* ((nxt-byte (read-byte stream)))
     (if (eq nxt-byte t/fco-instr-end)
         (return)
         (let* ((opc (aref op-name-mapping (- nxt-byte 127)))
                (opc-str (symbol-name opc))
                (new-str (substitute #\- #\/ opc-str :count 1))
                (opc-new (read-from-string new-str))
                (opc-list (list opc-new)))
           (if (is-opcode1 nxt-byte)
               ;; push the arg in - which means list is reversed
               (push (read-byte stream) opc-list))
           (push (reverse opc-list) (instr fco)))
     )))
  ;; instruction are added in reverse order, so correct it
  (setf (instr fco) (reverse (instr fco)))

  fco
  )

(defun deserialize-co-from-file (filename)
  (with-open-file (stream filename
                          :direction :input
                          :element-type '(unsigned-byte 8))

    (let ((magic (make-sequence 'list 8)))
      (read-sequence magic stream)
      (assert (equal magic *magic*)))

    ; expect start of a single fco
    ; in the future it could be anything else, or repeated
    (assert (eq (read-byte stream) t/fco-start))

    (deserialize-fco-from-stream stream)
    ))

(deserialize-co-from-file "sample.gaja")

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
