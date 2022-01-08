(defpackage clop
  (:use #:cl
        #:clop.rules
        #:clop.toml-block-parser)
  (:export
   ;; clop.toml-block-parser
   #:parse
   #:parse-toml-blocks
   #:collection
   #:children
   #:table
   #:inline-table
   #:table-array
   #:toml-redefine-table-error
   #:toml-redefine-property-error
   #:toml-modify-inline-table-error
   #:toml-dotted-key-redefine-table-error
   #:toml-dotted-key-open-table-array-error))

(in-package clop)

(defun parse (text &key (style :alist))
  "Parse given string TEXT and convert the result to given STYLE.
The STYLE can be one of:
* :alist (the default)
* :jsown (jsown-like object)
* :raw (should be rarely used)

The top-level of result is an alist.

You may implement your own style by implementing SERIALIZE method."
  (let* ((parsed (esrap:parse 'toml text)))
    (serialize parsed style)))

(defgeneric serialize (table style)
  (:documentation "Convert given TABLE to STYLE.
The TABLE should be the root table containing "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            JSOWN                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod serialize ((table table) (style (eql :jsown)))
  (cons :obj
        (loop with children = (children table)
              for key being the hash-keys of children
              collect (cons key (serialize (gethash key children) style)))))

(defmethod serialize ((table inline-table) (style (eql :jsown)))
  (cons :obj
        (loop with children = (children table)
              for key being the hash-keys of children
              collect (cons key (serialize (gethash key children) style)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            ALIST                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod serialize ((table table) (style (eql :alist)))
  (loop with children = (children table)
        for key being the hash-keys of children
        collect (cons key (serialize (gethash key children) style))))

(defmethod serialize ((table inline-table) (style (eql :alist)))
  (loop with children = (children table)
        for key being the hash-keys of children
        collect (cons key (serialize (gethash key children) style))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Common                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod serialize ((table table-array) style)
  (mapcar (lambda (it) (serialize it style))
          (children table)))

(defmethod serialize (thing style)
  thing)

(defmethod serialize ((thing list) style)
  (if (listp (cdr thing))
      (mapcar (lambda (it) (serialize it style)) thing)
      thing))

(defmethod serialize (thing (style (eql :raw)))
  thing)
