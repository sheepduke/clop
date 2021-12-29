(defsystem clop
  :version "0.1.0"
  :description "CLOP - Common Lisp tOml Parser"
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:alexandria
               #:esrap
               #:parse-number
               #:local-time
               #:str)
  :serial t
  :components ((:module "src"
                :components
                ((:file "conditions")
                 (:file "toml-value-parser")
                 (:file "config")
                 (:file "toml-block")
                 (:file "toml-block-parser")
                 (:file "rules")
                 (:file "clop"))))
  :in-order-to ((test-op (test-op :clop-tests))))

(defsystem clop-tests
  :version "0.1.0"
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:clop
               #:fiveam)
  :serial t
  :components ((:module "tests"
                :components
                ((:file "clop")
                 (:file "rules"))))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :clop :clop-tests))))
