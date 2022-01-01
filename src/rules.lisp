(defpackage clop.rules
  (:use #:cl
        #:esrap
        #:clop.toml-block
        #:clop.conditions)
  (:local-nicknames (#:config #:clop.config)
                    (#:time #:local-time)
                    (#:block-parser #:clop.toml-block-parser))
  (:import-from #:alexandria
                #:compose
                #:assoc-value
                #:make-keyword
                #:symbolicate)
  (:export #:toml))

(in-package clop.rules)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             TOML                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule toml
    (and (* (or whitespace comment newline))
         (? (and toml-block
                 (* (or whitespace comment))))
         (* (and (* (or whitespace comment))
                 newline
                 (* (or whitespace comment newline))
                 (? toml-block)
                 (* (or whitespace comment)))))
  (:destructure (_1 first-block blocks)
    (declare (ignore _1))
    (let* ((first-blocks (if first-block (list (first first-block)) nil))
           (rest-blocks-with-nil (mapcar (lambda (definition)
                                           (fourth definition))
                                         blocks))
           (rest-blocks (remove-if #'null rest-blocks-with-nil)))
      (block-parser:parse-toml-blocks (append first-blocks rest-blocks)))))

(defrule toml-block (or key-value-pair table))

(defrule comment (and #\# (* (comment-char-p character)))
  (:constant nil))

(defun comment-char-p (char)
  (let ((code (char-code char)))
    (or (= code #x09)
        (<= #x20 code #x7E)
        (non-ascii-p char))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Table                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule table (or named-table array-table))

(defrule named-table
    (and (and "[" (* whitespace))
         key
         (and (* whitespace) "]"))
  (:destructure (_1 names _2)
    (declare (ignore _1 _2))
    (make-instance 'toml-named-table
                   :names names)))

(defrule array-table
    (and (and "[[" (* whitespace))
         key
         (and (* whitespace) "]]"))
  (:destructure (_1 names _2)
    (declare (ignore _1 _2))
    (make-instance 'toml-array-table
                   :names names)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                        Key Value Pair                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule key-value-pair (and key (* whitespace) "=" (* whitespace) value)
  (:destructure (key _1 _2 _3 value)
    (declare (ignore _1 _2 _3))
    (make-instance 'toml-key-value-pair
                   :keys key
                   :value value)))

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

(defrule dotted-key
    (and simple-key (+ (and (* whitespace) "." (* whitespace) simple-key)))
  (:destructure (first rest)
    ;; Return keys as a list.
    (append (list first) (mapcar (lambda (it) (fourth it)) rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Value                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule value
    (or array
        inline-table
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
  (:lambda (value) (parse-value :integer value)))

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
  (:lambda (value) (parse-value :float value)))

;; Normal values.
(defrule float-normal
    (or (and float-int float-fraction (? float-exp))
        (and float-int float-exp))
  (:text t)
  (:lambda (text)
    (let ((*read-default-float-format* 'double-float))
      (parse-number text))))

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
      (parse-value :bool value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            String                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule string
    (or multiline-basic-string
        basic-string
        multiline-literal-string
        literal-string)
  (:text t)
  (:lambda (value) (parse-value :string value)))

;; Basic string.

(defrule basic-string (and "\"" (* basic-char) "\"")
  (:destructure (_1 text _2)
    (declare (ignore _1 _2))
    text))

(defrule basic-char
    (or escaped-char
        (basic-unescaped-char-p character)))

(defrule escaped-char
    (and "\\" (or (and "u" (string 4))
                  (and "U" (string 8))
                  character))
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
       (#\u (parse-utf8 (subseq text 2)))
       (#\U (parse-utf8 (subseq text 2)))
       (t (error 'toml-invalid-text-error :text text))))))

(defun parse-utf8 (text)
  (or (every #'hexp text)
      (error 'toml-invalid-utf8-error :text text))
  (let ((code (parse-integer text :radix 16)))
    (or (<= #x0000 code #x007F)
        (<= #x0080 code #x07FF)
        (<= #x0800 code #x0FFF)
        (<= #x1000 code #xCFFF)
        (<= #xD000 code #xD7FF)
        (<= #xE000 code #xFFFF)
        (<= #x10000 code #x3FFFF)
        (<= #x40000 code #xFFFFF)
        (<= #x100000 code #x10FFFF)
        (error 'toml-invalid-utf8-error :text text))
    (code-char code)))

(defun basic-unescaped-char-p (char)
  (let ((code (char-code char)))
    (or (char= char #\space)
        (char= char #\tab)
        (= code #x21)
        (<= #x23 code #x5B)
        (<= #x5D code #x7E)
        (non-ascii-p char))))

;; Multi-line string.

(defrule multiline-basic-string
    (and "\"\"\"" (? newline) multiline-basic-body "\"\"\"")
  (:destructure (_1 _2 text _3)
    (declare (ignore _1 _2 _3))
    text))

(defrule multiline-basic-body
    (* (or escaped-char
           escaped-newline
           (multiline-basic-text-p (not "\"\"\""))))
  (:text t))

(defun multiline-basic-text-p (char)
  (let ((code (char-code char)))
    (or (char= char #\space)
        (char= char #\tab)
        (char= char #\newline)
        (char= char #\")
        (= code #x21)
        (<= #x23 code #x5B)
        (<= #x5D code #x7E)
        (non-ascii-p char)
        (error 'toml-invalid-text-error :text char))))

(defrule escaped-newline
    (and "\\" (* whitespace) newline (* (or whitespace #\newline)))
  (:text t)
  (:constant ""))

;; Literal string.

(defrule literal-string (and "'" (* (literal-char-p character)) "'")
  (:destructure (_1 text _2)
    (declare (ignore _1 _2))
    text))

(defun literal-char-p (char)
  (let ((code (char-code char)))
    (or (= #x09 code)
        (<= #x20 code #x26)
        (<= #x28 code #x7E)
        (non-ascii-p char))))

;; Literal multi-line string.

(defrule multiline-literal-string
    (and "'''"
         (? newline)
         (* (not "'''"))
         "'''")
  (:destructure (_1 _2 text _3)
    (declare (ignore _1 _2 _3))
    (or (every #'literal-char-p text)
        (error 'toml-invalid-text-error :text text))
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
  (:lambda (text) (parse-value :datetime text)))

(defrule local-date-time (and full-date date-time-delimeter partial-time)
  (:text t)
  (:lambda (text) (parse-value :datetime-local text)))

(defrule local-date full-date
  (:text t)
  (:lambda (text) (parse-value :date-local text)))

(defrule local-time partial-time
  (:text t)
  (:lambda (text) (parse-value :time-local text)))

(defrule full-date (and date-year "-" date-month "-" date-day))

(defrule full-time (and partial-time time-offset))

(defrule partial-time
    (and time-hour ":" time-minute ":" time-second (? time-second-fraction)))

(defrule date-year (and digit digit digit digit))

(defrule date-month (and digit digit))

(defrule date-day (and digit digit))

(defrule date-time-delimeter (or "t" "T" " "))

(defrule time-hour (and digit digit))

(defrule time-minute (and digit digit))

(defrule time-second (and digit digit))

(defrule time-second-fraction (and "." (+ digit)))

(defrule time-offset (or "z" "Z" time-num-offset))

(defrule time-num-offset (and (or "+" "-") time-hour ":" time-minute))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Array                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule array (and "[" (? array-values) (? array-omitted-content) "]")
  (:destructure (_1 values _2 _3)
    (declare (ignore _1 _2 _3))
    values))

(defrule array-values
    (and (? array-omitted-content)
         value
         (* array-comma-element)
         (? ","))
  (:destructure (_1 first rest _2)
    (declare (ignore _1 _2))
    (cons first rest)))

(defrule array-comma-element
    (and (? array-omitted-content) "," (? array-omitted-content) value)
  (:destructure (_1 _2 _3 value)
    (declare (ignore _1 _2 _3))
    value))

(defrule array-omitted-content
    (* (or whitespace (and (? comment) newline))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                         Inline Table                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule inline-table
    (and (and "{" (* whitespace))
         (? inline-table-key-values)
         (and (* whitespace) "}"))
  (:destructure (_1 key-value-pairs _2)
    (declare (ignore _1 _2))
    (make-instance 'toml-inline-table :pairs key-value-pairs)))

(defrule inline-table-key-values
    (and key-value-pair
         (* (and (* whitespace) "," (* whitespace) key-value-pair)))
  (:destructure (first-pair rest-matches)
    (append (list first-pair)
            (mapcar (lambda (it) (fourth it)) rest-matches))))

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

(defrule hex (hexp character))

(defun hexp (char)
  (or (char<= #\0 char #\9)
      (char<= #\a char #\f)
      (char<= #\A char #\F)))

(defun non-ascii-p (char)
  (let ((code (char-code char)))
    (or (<= #x80 code #xD7FF)
        (<= #xE000 code #x10FFF))))

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

(defun parse-value (type text)
  (funcall config:*value-parser* type text))
