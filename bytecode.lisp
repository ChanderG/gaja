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

(defclass co ()
  ((name :initarg :name :accessor name)
   (funcs :initarg :funcs :accessor funcs)))

(defun make-co (name funcs)
  (make-instance 'co :name name :funcs funcs))

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

; end of an array of numbers
(deftag t/num-array-end 30)
; end of an array of strings
(deftag t/str-array-end 31)

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

(defun ser-number-to-stream (stream num)
  (cond ((and (>= num 0) (< num 256))
         (write-sequence (list t/small-uint num) stream))
        ((and (< num 0) (> num -256))
         (write-sequence (list t/small-nint (* -1 num)) stream))
        (t (error (format nil "Unhandled number: ~d" num)))))

(defun ser-str-to-stream (stream str)
  (let* ((len (length str)))
    (cond
      ((eq len 1)
       (write-byte t/char stream)
       (write-byte (char-code (char str 0)) stream))
      ((< len 256)
       (write-byte t/short-string stream)
       (write-byte len stream)
       (write-sequence (map 'list 'char-code str) stream))
      (t (error "Unhandled string format")))))

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
    (ser-str-to-stream stream (symbol-name (name fco)))

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

    ;; write out the constants
    (dolist (c (consts fco))
      (ser-number-to-stream stream c))
    ;; followed by an end of array tag
    (write-byte t/num-array-end stream)

    ;; write out the variables
    (dolist (var (vars fco))
      ;; convert var symbol to string
      (ser-str-to-stream stream (symbol-name var)))
    (write-byte t/str-array-end stream)

    ;; end func co obj
    (write-byte t/fco-end stream)
    ))

(defun deser-number-from-stream (stream)
  (let* ((tag (read-byte stream)))
    (cond
      ((eq tag t/num-array-end) nil)
      ((eq tag t/small-uint)
       (read-byte stream))
      ((eq tag t/small-nint)
       (* -1 (read-byte stream)))
      (t (error "Unhandled number")))))

(defun deser-str-from-stream (stream)
  (let* ((tag (read-byte stream)))
    (cond
      ((eq tag t/str-array-end) nil)
      ((eq tag t/char)
       (string (code-char (read-byte stream))))
      ((eq tag t/short-string)
       (let* ((len (read-byte stream))
              (seq (make-sequence 'list len)))
         (read-sequence seq stream)
         (map 'string 'code-char seq)))
      (t (error "Unexpected string format")))))

(defun deserialize-fco-from-stream (stream &aux
                                             (fco (make-instance 'func-co)))
  ;; name of a function
  (setf (name fco) (deser-str-from-stream stream))

  ;; instructions
  ;; consumes the last end of instructions byte
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
           (push (reverse opc-list) (instr fco))))))
  ;; instruction are added in reverse order, so correct it
  (setf (instr fco) (reverse (instr fco)))

  ;; pull out the constants
  (loop
    (let* ((num (deser-number-from-stream stream)))
      (if num
          (push num (consts fco))
          (return))))
  (setf (consts fco) (reverse (consts fco)))

  ;; pull out the variables
  (loop
   (let* ((var (deser-str-from-stream stream)))
     (if var
         (push var (vars fco))
         (return))
     ))
  (setf (vars fco) (reverse (vars fco)))

  fco)

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
