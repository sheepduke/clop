(defsystem clomp
  :version "0.1.0"
  :description "CLOMP - Common Lisp tOMl Parser"
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:alexandria
               #:esrap
               #:parse-number
               #:str)
  :serial t
  :components ((:module "src"
                :components
                ((:file "clomp")
                 (:file "rules"))))
  :in-order-to ((test-op (test-op :clomp-tests))))

(defsystem clomp-tests
  :version "0.1.0"
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:clomp
               #:fiveam)
  :serial t
  :components ((:module "tests"
                :components
                ((:file "clomp")
                 (:file "rules"))))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :clomp :clomp-tests))))
