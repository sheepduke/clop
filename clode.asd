(defsystem clode
  :version "0.1.0"
  :description "CLODE - Common Lisp tOMl Parser"
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
                ((:file "util")
                 (:file "config")
                 (:file "rules")
                 (:file "clode"))))
  :in-order-to ((test-op (test-op :clode-tests))))

(defsystem clode-tests
  :version "0.1.0"
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:clode
               #:fiveam)
  :serial t
  :components ((:module "tests"
                :components
                ((:file "clode")
                 (:file "rules"))))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :clode :clode-tests))))
