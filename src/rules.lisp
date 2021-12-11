(defpackage clomp.rules
  (:use #:cl)
  (:use #:esrap))

(in-package clomp.rules)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Integer                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule integer (or hex-int
                     oct-int
                     bin-int
                     decimal-int))

;; Decimal.
(defrule decimal-int
    (and (? (or plus minus))
         decimal-unsigned-int)
  (:text t)
  (:lambda (text) (parse-number text)))

(defrule decimal-unsigned-int
    (or (and digit-1-9
             (+ (and (? underscore) digit)))
        digit)
  (:text t))

(defrule minus #\-)

(defrule plus #\+)

(defrule digit-1-9 (character-ranges (#\1 #\9)))

;; Hex.
(defrule hex-int
    (and hex-prefix
         hex-digit
         (* (and (? underscore) hex-digit)))
  (:text t)
  (:lambda (text) (parse-number text :radix 16 :start 2)))

(defrule hex-prefix "0x")

;; Oct.
(defrule oct-int
    (and oct-prefix
         oct-digit
         (* (and (? underscore) oct-digit)))
  (:text t)
  (:lambda (text) (parse-number text :radix 8 :start 2)))

(defrule oct-prefix "0o")

(defrule oct-digit (character-ranges (#\0 #\7)))

;; Binary.
(defrule bin-int
    (and bin-prefix
         bin-digit
         (* (and (? underscore) bin-digit)))
  (:text t)
  (:lambda (text) (parse-number text :radix 2 :start 2)))

(defrule bin-prefix "0b")

(defrule bin-digit (character-ranges #\0 #\1))

;; Common rules.
(defrule underscore #\_)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Basic                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule alpha
    (or (character-ranges (#\a #\z))
        (character-ranges (#\A #\Z))))

(defrule digit
    (character-ranges (#\0 #\9)))

(defrule hex-digit
    (or digit
        (character-ranges (#\a #\f))
        (character-ranges (#\A #\F))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Functions                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-number (string &key (radix 10) (start 0))
  "Parse number from STRING with underscores dropped."
  (parse-number:parse-number (str:replace-all "_" "" string)
                             :radix radix
                             :start start))
