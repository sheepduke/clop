(defpackage clop.toml-block
  (:use #:cl)
  (:export #:toml-block
           #:toml-key-value-pair
           #:keys
           #:value
           #:toml-table
           #:pairs
           #:toml-named-table
           #:names
           #:toml-inline-table
           #:toml-array-table))

(in-package clop.toml-block)

(defclass toml-block () ())

(defclass toml-key-value-pair (toml-block)
  ((keys :type list :initarg :keys :accessor keys)
   (value :accessor value :initarg :value)))

(defclass toml-table (toml-block) ())

(defclass toml-named-table (toml-table)
  ((names :type string :initarg :names :accessor names)))

(defclass toml-inline-table (toml-table)
  ((pairs :type list :initarg :pairs :accessor pairs)))

(defclass toml-array-table (toml-named-table) ())

(defmethod print-object ((obj toml-key-value-pair) stream)
  (format stream "#KeyValuePair(~a . ~a)"
          (keys obj)
          (value obj)))

(defmethod print-object ((table toml-named-table) stream)
  (format stream "#NamedTable(~a)" (names table)))

(defmethod print-object ((table toml-inline-table) stream)
  (format stream "#InlineTable(~a)" (pairs table)))

(defmethod print-object ((table toml-array-table) stream)
  (format stream "#ArrayTable(~a)" (names table)))
