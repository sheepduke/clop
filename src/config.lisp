(defpackage clop.config
  (:use #:cl)
  (:export #:*value-true*
           #:*value-false*
           #:*value-+inf*
           #:*value--inf*
           #:*value-+nan*
           #:*value--nan*
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

(defvar *value-parser* #'clop.toml-value-parser:parse-value)
