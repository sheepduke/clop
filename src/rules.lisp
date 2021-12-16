(defpackage clop.rules
  (:use #:cl
        #:esrap)
  (:local-nicknames (#:config #:clop.config)
                    (#:time #:local-time))
  (:import-from #:alexandria
                #:make-keyword
                #:symbolicate))

(in-package clop.rules)

(defrule value
    (or string
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
        decimal-int))

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

(defrule float (or float-special float-normal))

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
    (if (string= "true" text)
        config:*value-true*
        config:*value-false*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            String                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule string
    (or multiline-basic-string
        basic-string
        multiline-literal-string
        literal-string)
  (:text t))

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
    (and "\"\"\"" (? #\newline) multiline-basic-body "\"\"\"")
  (:destructure (left-quote newline text right-quote)
    (declare (ignore left-quote newline right-quote))
    text))

(defrule multiline-basic-body
    (* (or escaped-char
           escaped-newline
           (not "\"\"\"")))
  (:text t))

(defrule escaped-newline
    (and "\\" (* whitespace) #\newline (* (or whitespace #\newline)))
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
    (funcall #'time:parse-timestring
             (str:replace-first " " "T" text))))

(defrule local-date-time (and full-date date-time-delimeter partial-time)
  (:text t)
  (:lambda (text) (funcall config:*local-date-time-parser* text)))

(defrule local-date full-date
  (:text t)
  (:lambda (text) (funcall config:*local-date-parser* text)))

(defrule local-time partial-time
  (:text t)
  (:lambda (text) (funcall config:*local-time-parser* text)))

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
;;;;                            Basic                             ;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Functions                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-number (string &key (radix 10) (start 0))
  "Parse number from STRING with underscores dropped."
  (parse-number:parse-number (str:replace-all "_" "" string)
                             :radix radix
                             :start start))
