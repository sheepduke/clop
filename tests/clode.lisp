(defpackage clode-tests
  (:use #:cl)
  (:import-from #:fiveam
                #:def-suite*)
  (:export #:clode))

(in-package clode-tests)

(def-suite* clode)
