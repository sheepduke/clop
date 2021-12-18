(defpackage clop.rules
  (:use #:cl
        #:esrap)
  (:local-nicknames (#:config #:clop.config)
                    (#:time #:local-time))
  (:import-from #:alexandria
                #:make-keyword
                #:symbolicate)
  (:export toml))

(in-package clop.rules)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             TOML                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule toml
    (and (* (or whitespace comment newline))
         (? (and key-value-pair-list newline))
         (* (or whitespace comment newline))
         named-table
         (* (or whitespace comment newline))))

(defrule comment (and #\# (* (not newline)))
  (:constant nil))

(defrule named-table
    (and "[" key "]"
         (* (or whitespace comment))
         newline
         key-value-pair-list))

(defrule key-value-pair-list
    (and key-value-pair (* (or whitespace comment))
         (* (and (+ newline) key-value-pair (* (or whitespace comment))))))

(defrule key-value-pair (and key (* whitespace) "=" (* whitespace) value)
  (:destructure (key whitespace equal-sign whitespace2 value)
    (declare (ignore whitespace equal-sign whitespace2))
    (cons key value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             Key                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule key (or dotted-key simple-key)
  (:lambda (key)
    ;; Return keys as a list.
    (if (listp key) key (list key))))

(defrule simple-key (or quoted-key unquoted-key)
  (:text t))

(defrule quoted-key (or basic-string literal-string))

(defrule unquoted-key (+ (or alpha digit "-" "_")))

(defrule dotted-key (and simple-key (+ (and "." simple-key)))
  (:destructure (first rest)
    ;; Return keys as a list.
    (append (list first)
            (mapcar (lambda (match) (cadr match)) rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Value                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule value
    (or array
        string
        boolean
        date-time
        float
        integer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Integer                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule integer
    (or hex-int
        oct-int
        bin-int
        decimal-int)
  (:lambda (value) (value-or-alist :integer value)))

;; Decimal.
(defrule decimal-int
    (and optional-sign
         decimal-unsigned-int)
  (:text t)
  (:lambda (text) (parse-number text)))

(defrule decimal-unsigned-int
    (or (and digit-1-9
             (+ (and optional-underscore digit)))
        digit))

(defrule digit-1-9 (character-ranges (#\1 #\9)))

;; Hex.
(defrule hex-int
    (and hex-prefix
         hex
         (* (and optional-underscore hex)))
  (:text t)
  (:lambda (text) (parse-number text :radix 16 :start 2)))

(defrule hex-prefix "0x")

;; Oct.
(defrule oct-int
    (and oct-prefix
         oct-digit
         (* (and optional-underscore oct-digit)))
  (:text t)
  (:lambda (text) (parse-number text :radix 8 :start 2)))

(defrule oct-prefix "0o")

(defrule oct-digit (character-ranges (#\0 #\7)))

;; Binary.
(defrule bin-int
    (and bin-prefix
         bin-digit
         (* (and optional-underscore bin-digit)))
  (:text t)
  (:lambda (text) (parse-number text :radix 2 :start 2)))

(defrule bin-prefix "0b")

(defrule bin-digit (character-ranges #\0 #\1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Float                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule float (or float-special float-normal)
  (:lambda (value) (value-or-alist :float value)))

;; Normal values.
(defrule float-normal
    (or (and float-int float-fraction (? float-exp))
        (and float-int float-exp))
  (:text t)
  (:lambda (text) (parse-number text)))

(defrule float-int (and optional-sign decimal-unsigned-int))

(defrule float-fraction (and "." digit-string))

(defrule float-exp
    (and (or "e" "E") optional-sign digit-string))

;; Special values.
(defrule float-special
    (and optional-sign
         (or "inf" "nan"))
  (:destructure (sign text)
    (let ((positivep (or (null sign) (string= sign "+")))
          (infp (string= text "inf")))
      (if infp
          (if positivep config:*value-+inf* config:*value--inf*)
          (if positivep config:*value-+nan* config:*value--nan*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Boolean                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule boolean (or "true" "false")
  (:text t)
  (:lambda (text)
    (let ((value (if (string= "true" text)
                     config:*value-true*
                     config:*value-false*)))
      (value-or-alist :bool value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            String                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule string
    (or multiline-basic-string
        basic-string
        multiline-literal-string
        literal-string)
  (:text t)
  (:lambda (value) (value-or-alist :string value)))

;; Basic string.

(defrule basic-string (and "\"" (* basic-char) "\"")
  (:destructure (left-quote text right-quote)
    (declare (ignore left-quote right-quote))
    text))

(defrule basic-char (or escaped-char basic-unescaped-char))

(defrule escaped-char
    (and "\\" (or (and "u" (string 4))
                  (and "U" (string 8))
                  "b" "t" "r" "n" "f" "\"" "\\"))
  (:text t)
  (:lambda (text)
    (string
     (case (elt text 1)
       (#\b #\backspace)
       (#\t #\tab)
       (#\r #\return)
       (#\n #\newline)
       (#\f #\page)
       (#\" #\")
       (#\\ #\\)
       (#\u (code-char (parse-integer (subseq text 2) :radix 16)))
       (#\U (code-char (parse-integer (subseq text 2) :radix 16)))))))

(defrule basic-unescaped-char (not "\""))

;; Multi-line string.

(defrule multiline-basic-string
    (and "\"\"\"" (? newline) multiline-basic-body "\"\"\"")
  (:destructure (left-quote newline text right-quote)
    (declare (ignore left-quote newline right-quote))
    text))

(defrule multiline-basic-body
    (* (or escaped-char
           escaped-newline
           (not "\"\"\"")))
  (:text t))

(defrule escaped-newline
    (and "\\" (* whitespace) newline (* (or whitespace #\newline)))
  (:text t)
  (:constant ""))

;; Literal string.

(defrule literal-string (and "'" (* (not "'")) "'")
  (:destructure (left-quote text right-quote)
    (declare (ignore left-quote right-quote))
    text))

;; Literal multi-line string.

(defrule multiline-literal-string (and "'''" (* (not "'''")) "'''")
  (:destructure (left-quote text right-quote)
    (declare (ignore left-quote right-quote))
    text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Date Time                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule date-time
    (or offset-date-time
        local-date-time
        local-date
        local-time))

(defrule offset-date-time (and full-date date-time-delimeter full-time)
  (:text t)
  (:lambda (text)
    (let ((value (funcall #'time:parse-timestring
                          (str:replace-first " " "T" text))))
      (value-or-alist :datetime value))))

(defrule local-date-time (and full-date date-time-delimeter partial-time)
  (:text t)
  (:lambda (text)
    (let ((value (funcall config:*local-date-time-parser* text)))
      (value-or-alist :datetime-local value))))

(defrule local-date full-date
  (:text t)
  (:lambda (text)
    (let ((value (funcall config:*local-date-parser* text)))
      (value-or-alist :date-local value))))

(defrule local-time partial-time
  (:text t)
  (:lambda (text)
    (let ((value (funcall config:*local-time-parser* text)))
      (value-or-alist :time-local value))))

(defrule full-date (and date-year "-" date-month "-" date-day))

(defrule full-time (and partial-time time-offset))

(defrule partial-time
    (and time-hour ":" time-minute ":" time-second (? time-second-fraction)))

(defrule date-year (and digit digit digit digit))

(defrule date-month (and digit digit))

(defrule date-day (and digit digit))

(defrule date-time-delimeter (or "T" " "))

(defrule time-hour (and digit digit))

(defrule time-minute (and digit digit))

(defrule time-second (and digit digit))

(defrule time-second-fraction (and "." (+ digit)))

(defrule time-offset (or "Z" time-num-offset))

(defrule time-num-offset (and (or "+" "-") time-hour ":" time-minute))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Array                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule array (and "[" (? array-values) (? array-omitted-content) "]")
  (:destructure (left-bracket values comment right-bracket)
    (declare (ignore left-bracket comment right-bracket))
    values))

(defrule array-values
    (and (? array-omitted-content)
         value
         (* array-comma-element)
         (? ","))
  (:destructure (comment first rest comma)
    (declare (ignore comment comma))
    (cons first rest)))

(defrule array-comma-element
    (and (? array-omitted-content) "," (? array-omitted-content) value)
  (:destructure (comment1 comma comment2 value)
    (declare (ignore comment1 comma comment2))
    value))

(defrule array-omitted-content
    (* (or whitespace (and (? comment) newline))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            basic                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule alpha
    (or (character-ranges (#\a #\z))
        (character-ranges (#\A #\Z))))

(defrule digit
    (character-ranges (#\0 #\9)))

(defrule digit-string
    (and digit (* (and optional-underscore digit))))

(defrule hex
    (or digit
        (character-ranges (#\a #\f))
        (character-ranges (#\A #\F))))

(defrule optional-underscore (? "_"))

(defrule optional-sign (? (or "+" "-")))

(defrule whitespace (or #\space #\tab))

(defrule newline (and (? #\return) #\newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Functions                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-number (string &key (radix 10) (start 0))
  "Parse number from STRING with underscores dropped."
  (parse-number:parse-number (str:replace-all "_" "" string)
                             :radix radix
                             :start start))

(defun value-or-alist (type value)
  (declare (ignorable type))
  #+toml-test (list (cons :type (string-downcase type))
                    (cons :value (format nil "~a" value)))
  #-toml-test value)
