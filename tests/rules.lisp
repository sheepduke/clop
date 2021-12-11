(defpackage clomp-tests.rules
  (:use #:cl)
  (:local-nicknames (#:rules #:clomp.rules))
  (:import-from #:esrap
                #:parse)
  (:import-from #:fiveam
                #:is
                #:def-suite*
                #:test))

(in-package clomp-tests.rules)

(def-suite* clomp.rules :in clomp-tests:clomp)

(test integer
  ;; Decimal.
  (is (= 99 (parse 'rules::integer "+99")))
  (is (= 42 (parse 'rules::integer "42")))
  (is (= 0 (parse 'rules::integer "0")))
  (is (= 0 (parse 'rules::integer "+0")))
  (is (= 0 (parse 'rules::integer "-0")))
  (is (= 1 (parse 'rules::integer "1")))
  (is (= -17 (parse 'rules::integer "-17")))
  (is (= 1000 (parse 'rules::integer "1000")))
  (is (= 5349221 (parse 'rules::integer "5_349_221")))
  (is (= 5349221 (parse 'rules::integer "53_49_221")))
  (is (= 12345 (parse 'rules::integer "1_2_3_4_5")))
  (is (= -123 (parse 'rules::integer "-12_3")))

  ;; Hexadecimal.
  (is (= 26 (parse 'rules::integer "0x1a")))
  (is (= 44527 (parse 'rules::integer "0xAd_Ef")))

  ;; Octet.
  (is (= 0 (parse 'rules::integer "0o0")))
  (is (= 1 (parse 'rules::integer "0o1")))
  (is (= 10 (parse 'rules::integer "0o12")))

  ;; Binary.
  (is (= 0 (parse 'rules::integer "0b0")))
  (is (= 5 (parse 'rules::integer "0b0001_01"))))
