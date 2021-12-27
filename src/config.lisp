(defpackage clop.config
  (:use #:cl)
  (:local-nicknames (#:util #:clop.util))
  (:export #:*value-true*
           #:*value-false*
           #:*value-+inf*
           #:*value--inf*
           #:*value-+nan*
           #:*value--nan*
           #:*local-date-time-parser*
           #:*local-date-parser*
           #:*local-time-parser*
           #:*value-parser*))

(in-package clop.config)

(defvar *value-+inf* :+inf
  "The value of +inf when decoding TOML.")

(defvar *value--inf* :-inf
  "The value of -inf when decoding TOML.")

(defvar *value-+nan* :+nan
  "The value of +nan when decoding TOML.")

(defvar *value--nan* :-nan
  "The value of -nan when decoding TOML.")

(defvar *value-true* t
  "The value of true when decoding TOML.")

(defvar *value-false* nil
  "The value of false when decoding TOML.")

(defvar *value-parser*
  (lambda (type value) value))

(defvar *local-date-time-parser* #'util:parse-local-date-time
  "Function for parsing local date time of YYYY-MM-DDTHH:mm:ss etc etc.
The function takes a string as input and returns corresponding value.")

(defvar *local-date-parser* #'util:parse-local-date
  "Function for parsing local date of YYYY-MM-DD format.
The function takes a string as input and returns corresponding value.")

(defvar *local-time-parser* #'util:parse-local-time
  "Function for parsing local time of HH:mm:ss or HH:mm:ss.micro format.
The function takes a string as input and returns corresponding value.")
