(defpackage clop.conditions
  (:use #:cl)
  (:export #:toml-parse-error
           #:toml-invalid-text-error
           #:toml-invalid-unicode-error
           #:toml-table-error
           #:toml-redefine-table-error
           #:toml-redefine-property-error
           #:toml-modify-inline-table-error
           #:toml-dotted-key-redefine-table-error
           #:toml-dotted-key-open-table-array-error))

(in-package clop.conditions)

(define-condition toml-parse-error (error) ())

(define-condition toml-invalid-text-error (toml-parse-error)
  ((text :accessor text :initarg :text))
  (:report (lambda (condition stream)
             (format stream
                     "Invalid text ~a detected"
                     (text condition)))))

(define-condition toml-invalid-unicode-error (toml-invalid-text-error) ()
  (:report (lambda (condition stream)
             (format stream
                     "Invalid unicode ~a detected"
                     (text condition)))))

(define-condition toml-table-error (error)
  ((names :accessor names :initarg :names)))

(define-condition toml-redefine-table-error (toml-parse-error) ()
  (:report (lambda (condition stream)
             (format stream
                     "Table name ~a is already defined"
                     (names condition)))))

(define-condition toml-redefine-property-error (toml-parse-error) ()
  (:report (lambda (condition stream)
             (format stream
                     "Property name ~a is already defined"
                     (names condition)))))

(define-condition toml-modify-inline-table-error (toml-parse-error) ()
  (:report (lambda (condition stream)
             (format stream
                     "Inline table ~a cannot be modified once defined"
                     (names condition)))))

(define-condition toml-dotted-key-redefine-table-error (toml-parse-error) ()
  (:report (lambda (condition stream)
             (format stream
                     "Dotted key ~a cannot redefine table defined by [Table] header or dotted key from another section"
                     (names condition)))))

(define-condition toml-dotted-key-open-table-array-error (toml-parse-error) ()
  (:report (lambda (condition stream)
             (format stream
                     "Dotted key ~a cannot open table array"
                     (names condition)))))
