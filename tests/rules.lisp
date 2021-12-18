(defpackage clop-tests.rules
  (:use #:cl)
  (:local-nicknames (#:rules #:clop.rules)
                    (#:config #:clop.config)
                    (#:time #:local-time))
  (:import-from #:esrap
                #:parse
                #:esrap-parse-error)
  (:import-from #:fiveam
                #:signals
                #:is
                #:def-suite*
                #:test))

(in-package clop-tests.rules)

(def-suite* clop.rules :in clop-tests:clop)

(test comment
  (is (null (parse 'rules::comment "# comment here")))
  (is (null (parse 'rules::comment "#"))))

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
  (is (equal :+inf (parse 'rules::value "inf")))
  (is (equal :+inf (parse 'rules::value "+inf")))
  (is (equal :-inf (parse 'rules::value "-inf")))
  (is (equal :+nan (parse 'rules::value "nan")))
  (is (equal :+nan (parse 'rules::value "+nan")))
  (is (equal :-nan (parse 'rules::value "-nan"))))

(test value-boolean
  (is (equal t (parse 'rules::value "true")))
  (is (equal nil (parse 'rules::value "false")))

  (let ((config:*value-true* :true)
        (config:*value-false* :false))
    (is (equal :true (parse 'rules::value "true")))
    (is (equal :false (parse 'rules::value "false")))))

(test value-datetime
  (is (time:timestamp= (time:parse-timestring "2012-12-30T01:23:45Z")
                       (parse 'rules::value "2012-12-30T01:23:45Z")))
  (is (equal '(:year 2021 :month 10 :day 12)
             (parse 'rules::value "2021-10-12")))
  (is (equal '(:hour 10 :minute 12 :second 34 :microsecond 560)
             (parse 'rules::value "10:12:34.000560")))
  (is (equal '(:year 2021 :month 10 :day 12 :hour 10 :minute 12 :second 34 :microsecond 210000)
             (parse 'rules::value "2021-10-12 10:12:34.210000"))))

(test value-string
  ;; Basic string.
  (is (string= "hello" (parse 'rules::value "\"hello\"")))
  (is (string= "δ" (parse 'rules::value "\"\\U000003B4\"")))
  (is (string= "δ" (parse 'rules::value "\"\\u03B4\"")))
  (is (string= "\\" (parse 'rules::value "\"\\\\\"")))

  ;; Multi-line string.
  (is (string= "The quick brown fox jumps over the lazy dog."
               (parse 'rules::value "\"\"\"
The quick brown \\


  fox jumps over \\
    the lazy dog.\"\"\""))
      (string= "The quick brown fox jumps over the lazy dog."
               (parse 'rules::value "\"\"\"\\
       The quick brown \\
       fox jumps over \\
       the lazy dog.\\
       \"\"\"")))

  ;; String literal.
  (is (string= "C:\\Users\\nodejs\\templates"
               (parse 'rules::value "'C:\\Users\\nodejs\\templates'")))
  (is (string= "Tom \"Dubs\" Preston-Werner"
               (parse 'rules::value "'Tom \"Dubs\" Preston-Werner'")))
  (is (string= "I [dw]on't need \\d{2} apples"
               (parse 'rules::value "'''I [dw]on't need \\d{2} apples'''"))))

(test value-array
  (is (equal '(1 2 3)
             (parse 'rules::value "[1,2,3]")))
  (is (equal '()
             (parse 'rules::value "[]")))
  (is (equal '((1 2) ("a" "b" "c"))
             (parse 'rules::value "[ [ 1, 2 ], [\"a\", \"b\", \"c\"] ]")))
  (is (equal '(1 2 3)
             (parse 'rules::value "[1, #comment
2 #comment,
, 3,
]"))))

(test key
  (is (equal '("s1mple-key_2")
             (parse 'rules::key "s1mple-key_2")))
  (is (equal '("animal" "dog" "name" "123")
             (parse 'rules::key "animal.\"dog\" . name.123"))))

(test key-value-pair
  (is (equal '(("1") . 2)
             (parse 'rules::key-value-pair "1 = 2")))
  (is (equal '(("dog" "name") . "Bob")
             (parse 'rules::key-value-pair "dog.name = \"Bob\""))))

(test key-value-pair-list
  (is (equal '((("fish" "age") . 7)
               (("cat" "name") . "Alice")
               (("dog" "name") . "Bob"))
             (parse 'rules::key-value-pair-list "fish.age  =7
cat.name = \"Alice\" # Test comment here.

dog . name=   \"\"\"
Bob\"\"\"
"))))
