(defpackage clomp-tests
  (:use #:cl)
  (:import-from #:fiveam
                #:def-suite*)
  (:export #:clomp))

(in-package clomp-tests)

(def-suite* clomp)
