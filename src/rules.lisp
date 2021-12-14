(defpackage clode.rules
  (:use #:cl
        #:esrap)
  (:local-nicknames (#:config #:clode.config))
  (:import-from #:alexandria
                #:make-keyword
                #:symbolicate))

(in-package clode.rules)

(defrule value
    (or boolean
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
         hex-digit
         (* (and optional-underscore hex-digit)))
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
          (if positivep config:*decoder-value-+inf* config:*decoder-value--inf*)
          (if positivep config:*decoder-value-+nan* config:*decoder-value--nan*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Boolean                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule boolean (or "true" "false")
  (:text t)
  (:lambda (text)
    (if (string= "true" text)
        config:*decoder-value-true*
        config:*decoder-value-false*)))

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

(defrule hex-digit
    (or digit
        (character-ranges (#\a #\f))
        (character-ranges (#\A #\F))))

(defrule optional-underscore (? "_"))

(defrule optional-sign (? (or "+" "-")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Functions                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-number (string &key (radix 10) (start 0))
  "Parse number from STRING with underscores dropped."
  (parse-number:parse-number (str:replace-all "_" "" string)
                             :radix radix
                             :start start))

(local-time:parse-timestring "07:32:00")

(local-time:parse-timestring "1979-05-27"
                             :allow-missing-time-part t)
