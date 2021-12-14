(defpackage clode.config
  (:use #:cl)
  (:local-nicknames (#:util #:clode.util))
  (:export #:*decoder-value-true*
           #:*decoder-value-false*
           #:*decoder-value-+inf*
           #:*decoder-value--inf*
           #:*decoder-value-+nan*
           #:*decoder-value--nan*
           #:*decoder-local-date-time-parser*
           #:*decoder-local-date-parser*
           #:*decoder-local-time-parser*))

(in-package clode.config)

(defvar *decoder-value-+inf* :+inf
  "The value of +inf when decoding TOML.")

(defvar *decoder-value--inf* :-inf
  "The value of -inf when decoding TOML.")

(defvar *decoder-value-+nan* :+nan
  "The value of +nan when decoding TOML.")

(defvar *decoder-value--nan* :-nan
  "The value of -nan when decoding TOML.")

(defvar *decoder-value-true* t
  "The value of true when decoding TOML.")

(defvar *decoder-value-false* nil
  "The value of false when decoding TOML.")

(defvar *decoder-local-date-time-parser* #'util:parse-local-date-time
  "Function for parsing local date time of YYYY-MM-DDTHH:mm:ss etc etc.
The function takes a string as input and returns corresponding value.")

(defvar *decoder-local-date-parser* #'util:parse-local-date
  "Function for parsing local date of YYYY-MM-DD format.
The function takes a string as input and returns corresponding value.")

(defvar *decoder-local-time-parser* #'util:parse-local-time
  "Function for parsing local time of HH:mm:ss or HH:mm:ss.micro format.
The function takes a string as input and returns corresponding value.")
