(defpackage clop.rules
  (:use #:cl
        #:esrap)
  (:local-nicknames (#:config #:clop.config)
                    (#:time #:local-time))
  (:import-from #:alexandria
                #:make-keyword
                #:symbolicate)
  (:import-from #:serapeum
                #:op
                #:~>>)
  (:export #:toml
           #:toml-parse-error
           #:message))

(in-package clop.rules)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Conditions                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition toml-parse-error (error)
  ((message :initarg :message :initform "" :accessor message)))

(define-condition toml-duplicated-key-error (toml-parse-error)
  ((message :initarg :message :initform "" :accessor message)))

(defmethod print-object ((err toml-parse-error) stream)
  (format stream "Error during parsing TOML: ~&~a" (message err)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Data Model                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass key-value-pair ()
  ((keys :type list :initarg :keys :accessor keys)
   (value :accessor value :initarg :value)))

(defclass table ()
  ((pairs :type list :initarg :pairs :accessor pairs)))

(defclass named-table (table)
  ((names :type string :initarg :names :accessor names)))

(defclass inline-table (table) ())

(defclass array-table (named-table) ())

(defmethod print-object ((obj key-value-pair) stream)
  (format stream "#KeyValuePair(~a . ~a)"
          (keys obj)
          (value obj)))

(defmethod print-object ((table named-table) stream)
  (format stream "#NamedTable(~a . ~a)" (names table) (pairs table)))

(defmethod print-object ((table inline-table) stream)
  (format stream "#InlineTable(~a)" (pairs table)))

(defmethod print-object ((table array-table) stream)
  (format stream "#ArrayTable(~a . ~a)"
          (names table)
          (pairs table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             TOML                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule toml
    (and
     (* (or whitespace comment newline))
     (? (and key-value-pair-list))
     (* (or whitespace comment newline))
     (* (and (or named-table array-table) (* (or whitespace comment newline))))
     (* (or whitespace comment newline)))
  (:destructure (_1 root-pairs _2 named-tables _3)
    (declare (ignore _1 _2 _3))
    (append root-pairs
            (mapcar (op (car _)) named-tables))))

(defrule comment (and #\# (* (not newline)))
  (:constant nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                         Named Table                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule named-table
    (and (and "[" (* whitespace))
         key
         (and (* whitespace) "]"
              (* (or whitespace comment))
              newline
              (* (or whitespace comment newline)))
         (? key-value-pair-list))
  (:destructure (_1 names _2 key-value-pair-list)
    (declare (ignore _1 _2))
    (make-instance 'named-table
                   :names names
                   :pairs key-value-pair-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                         Array Table                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule array-table
    (and (and "[[" (* whitespace))
         key
         (and (* whitespace) "]]"
              (* (or whitespace comment))
              newline
              (* (or whitespace comment newline)))
         (? key-value-pair-list))
  (:destructure (_1 names _2 key-value-pair-list)
    (declare (ignore _1 _2))
    (make-instance 'array-table
                   :names names
                   :pairs key-value-pair-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                        Key Value Pair                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule key-value-pair-list
    (and key-value-pair (* (or whitespace comment))
         (* (and (+ newline) key-value-pair (* (or whitespace comment))))
         (* newline))
  (:destructure (pair _1 more-pairs _2)
    (declare (ignore _1 _2))
    ;; (let* ((pairs (append (list pair)
    ;;                       (mapcar (op (cadr _)) more-pairs)))
    ;;        (keys (mapcar (op (car _)) pairs))
    ;;        (dup-keys (~>> pairs
    ;;                       (mapcar (op (let ((key-list (car _1)))
    ;;                                     (cons key-list
    ;;                                           (count key-list keys
    ;;                                                  :test #'equal)))))
    ;;                       (remove-duplicates _ :test #'equal)
    ;;                       (remove-if-not (op (> (cdr _) 1)))
    ;;                       (mapcar (op (str:join "." (car _1)))))))
    ;;   (unless (null dup-keys)
    ;;     (error 'toml-duplicated-key-error
    ;;            :message (format nil "Keys ~a are duplicated" dup-keys)))
    ;;   pairs)
    (append (list pair)
            (mapcar (op (cadr _)) more-pairs))))

(defrule key-value-pair (and key (* whitespace) "=" (* whitespace) value)
  (:destructure (key _1 _2 _3 value)
    (declare (ignore _1 _2 _3))
    (make-instance 'key-value-pair
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
    (append (list first)
            (mapcar (lambda (match) (cadddr match)) rest))))

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
  (:destructure (_1 text _2)
    (declare (ignore _1 _2))
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
  (:destructure (_1 _2 text _3)
    (declare (ignore _1 _2 _3))
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
  (:destructure (_1 text _2)
    (declare (ignore _1 _2))
    text))

;; Literal multi-line string.

(defrule multiline-literal-string (and "'''" (* (not "'''")) "'''")
  (:destructure (_1 text _2)
    (declare (ignore _1 _2))
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
    (make-instance 'inline-table :pairs key-value-pairs)))

(defrule inline-table-key-values
    (and key-value-pair
         (* (and (* whitespace) "," (* whitespace) key-value-pair)))
  (:destructure (first-pair rest-matches)
    (append (list first-pair)
            (mapcar (op (cadddr _)) rest-matches))))

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
