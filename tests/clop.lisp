(defpackage clop-tests
  (:use #:cl)
  (:import-from #:fiveam
                #:def-suite*)
  (:export #:clop))

(in-package clop-tests)

(def-suite* clop)
