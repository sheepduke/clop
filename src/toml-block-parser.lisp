(defpackage clop.toml-block-parser
  (:use #:cl
        #:clop.toml-block)
  (:local-nicknames (#:toml-block #:clop.toml-block))
  (:import-from #:serapeum
                #:~>>
                #:op)
  (:import-from #:alexandria
                #:hash-table-alist
                #:appendf)
  (:export
   #:parse-toml-blocks))

(in-package clop.toml-block-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Data Model                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass item () ())

(defclass table (item)
  ((children :accessor children
             :initform (make-hash-table :test #'equal)
             :documentation "The child elements of this table. It might be ")
   (parent :reader parent
           :initarg :parent
           :initform nil
           :documentation "Back reference to its parent table. For root table,
this slot is NIL.")))

(defclass inline-table (table) ())

(defclass table-array (table)
  ((children :initform (list))))

(defmethod print-object ((table table) stream)
  (format stream "#Table(~{~S~})"
          (hash-table-alist (children table))))

(defmethod print-object ((table inline-table) stream)
  (format stream "#InlineTable(~{~S~})"
          (hash-table-alist (children table))))

(defmethod print-object ((table table-array) stream)
  (format stream "#ArrayTable(~{~S~})"
          (children table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Conditions                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition toml-parse-error (error)
  ((message :initarg :message :initform "" :accessor message)))

(define-condition toml-duplicated-table-error (toml-parse-error) ())

(define-condition toml-duplicated-key-error (toml-parse-error)
  ((message :initarg :message :initform "" :accessor message)))

(defmethod print-object ((err toml-parse-error) stream)
  (format stream "Error during parsing TOML: ~&~a" (message err)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Parser                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass parser-context ()
  ((root-table :reader root-table
               :initform (make-instance 'table))
   (current-table :accessor current-table :initform nil)
   (tables-definitions :type hash-table
                       :accessor table-definitions
                       :initform (make-hash-table))))

(defun parse-toml-blocks (list)
  "Given a LIST of components (tables or key-value pairs), return an alist."
  (let ((context (make-instance 'parser-context)))
    (setf (current-table context) (root-table context))
    (mapc (op (parse-toml-block _ context)) list)
    (root-table context)))  

(defgeneric parse-toml-block (toml-block context))

(defmethod parse-toml-block ((pair toml-key-value-pair) context)
  (print "Invoked kv pair"))

(defmethod parse-toml-block ((toml-table toml-named-table) context)
  (let* ((names (toml-block:names toml-table)))
    (loop with current-table = (current-table context)
          with names-to-check = (butlast names)
          for name in names-to-check
          for table = (gethash name (children current-table))
          do (cond
               ((null table) (return current-table))
               ((typep table 'inline-table)
                (error 'toml-parse-error
                       :message (format nil
                                        "Inline table ~a cannot be modified"
                                        names)))
               (t (setf current-table table))))))

(defmethod parse-toml-block ((table toml-inline-table) context)
  (print "Invoked inline table"))

(defmethod parse-toml-block ((table toml-array-table) context)
  (print "Invoked array table"))

(defun define-new-table (names context)
  )

;; (loop with remaining-list = '(1 2 3 4)
;;       for first = (first remaining-list)
;;       for rest = (rest remaining-list)
;;       do (if (null rest)
;;              (print "last one found!"))
;;          (setf remaining-list rest))

(defun make-table-chain (first-table names)
  "Create a chain of tables specified by NAMES. The tables in path is
marked :created state, and the real (last) table is marked :defined."
  (loop with current-table = first-table
        for name in names
        for table = (make-instance 'table)
        do (setf (gethash name (children current-table)) table)
           (setf current-table table)
        finally (return current-table)))

(defun add-child-table (table child-table)
  (setf (children table)
        (append (children table) (list child-table))))

(esrap:parse 'clop.rules::toml
             (alexandria:read-file-into-string
              #P"/home/sheep/temp/silver-brain/config.toml"))
