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

;; tags from 0-9 reserved for special use later

;; 10-40 for fundamental types
; short string of length <255, so that the length is represented in a single byte
(deftag t/short-string 10)
; next byte refers directly to a single byte char, no length
(deftag t/char 11)
; next byte refers to a an int <255; no length
(deftag t/small-int 12)

;; 40 onwards for custom types
(deftag t/fco-start 40)
(deftag t/fco-end 41)

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
  (let* ((name "main"))
    (write-byte t/short-string stream)
    (write-byte 4 stream)
    (write-sequence (map 'list 'char-code name) stream)
    )

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
