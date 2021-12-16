(defpackage clop
  (:use #:cl)
  (:local-nicknames (#:rules #:clop.rules))
  (:export #:parse-string))

(in-package clop)

(defun parse-string (text)
  (esrap:parse 'rules:toml text))
