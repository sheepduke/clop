(defpackage clomp-tests.rules
  (:use #:cl)
  (:local-nicknames (#:rules #:clomp.rules))
  (:import-from #:esrap
                #:parse
                #:esrap-parse-error)
  (:import-from #:fiveam
                #:signals
                #:is
                #:def-suite*
                #:test))

(in-package clomp-tests.rules)

(def-suite* clomp.rules :in clomp-tests:clomp)

(test value-int
  ;; Decimal.
  (is (= 99 (parse 'rules::value "+99")))
  (is (= 42 (parse 'rules::value "42")))
  (is (= 0 (parse 'rules::value "0")))
  (is (= 0 (parse 'rules::value "+0")))
  (is (= 0 (parse 'rules::value "-0")))
  (is (= 1 (parse 'rules::value "1")))
  (is (= -17 (parse 'rules::value "-17")))
  (is (= 1000 (parse 'rules::value "1000")))
  (is (= 5349221 (parse 'rules::value "5_349_221")))
  (is (= 5349221 (parse 'rules::value "53_49_221")))
  (is (= 12345 (parse 'rules::value "1_2_3_4_5")))
  (is (= -123 (parse 'rules::value "-12_3")))

  ;; Hexadecimal.
  (is (= 26 (parse 'rules::value "0x1a")))
  (is (= 44527 (parse 'rules::value "0xAd_Ef")))

  ;; Octet.
  (is (= 0 (parse 'rules::value "0o0")))
  (is (= 1 (parse 'rules::value "0o1")))
  (is (= 10 (parse 'rules::value "0o12")))

  ;; Binary.
  (is (= 0 (parse 'rules::value "0b0")))
  (is (= 5 (parse 'rules::value "0b0001_01"))))

(test value-float
  ;; Normal floats.
  (is (= 1 (parse 'rules::value "+1.0")))
  (is (= 3.01234 (parse 'rules::value "3.01_234")))
  (is (= 0 (parse 'rules::value "+0.0")))
  (is (= 0 (parse 'rules::value "-0.0")))
  (is (= -0.01 (parse 'rules::value "-0.01")))
  (is (= -0.02 (parse 'rules::value "-2E-2")))
  (is (= 5e22 (parse 'rules::value "+5e+22")))

  ;; Invalid floats.
  (signals esrap-parse-error (parse 'rules::value "6.e+20"))
  
  ;; Special values.
  (is (equal :+inf (parse 'rules::value "+inf")))
  (is (equal :-inf (parse 'rules::value "-inf")))
  (is (equal :+nan (parse 'rules::value "+nan")))
  (is (equal :-nan (parse 'rules::value "-nan"))))
