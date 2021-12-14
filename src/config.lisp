(defpackage clode.config
  (:use #:cl)
  (:export
   #:*decoder-value-true*
   #:*decoder-value-false*
   #:*decoder-value-+inf*
   #:*decoder-value--inf*
   #:*decoder-value-+nan*
   #:*decoder-value--nan*))

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
