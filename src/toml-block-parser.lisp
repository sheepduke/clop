(defpackage clop.toml-block-parser
  (:use #:cl
        #:clop.conditions
        #:clop.toml-block)
  (:local-nicknames (#:toml-block #:clop.toml-block))
  (:import-from #:alexandria
                #:hash-table-alist
                #:appendf)
  (:export #:parse-toml-blocks
           #:collection
           #:children
           #:table
           #:inline-table
           #:table-array))

(in-package clop.toml-block-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Data Model                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass collection ()
  ((children :accessor children
             :initform (make-hash-table :test #'equal)
             :documentation "The child elements of this table. It might be ")))

(defclass table (collection)
  ((definition-context :type boolean
                       :accessor definition-context
                       :initarg :definition-context
                       :initform nil
                       :documentation "Indicates if the table is defined or not.
A table is defined in the following ways:
1. By [Table] header.
2. By being a path of dotted.key.tables. In this case, all the tables along the
way are created and defined.

Its value can be:
- T means defined via [Table] header.
- A table instance means defined under corresponding table section.")))

(defclass inline-table (collection) ())

(defclass table-array (collection)
  ((children :initform (list))))

(defmethod print-object ((table table) stream)
  (format stream "#Table(~{ ~S~})"
          (hash-table-alist (children table))))

(defmethod print-object ((table inline-table) stream)
  (format stream "#InlineTable(~{ ~S~})"
          (hash-table-alist (children table))))

(defmethod print-object ((table table-array) stream)
  (format stream "#ArrayTable(~{ ~S~})"
          (children table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Parser                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass parser-context ()
  ((root-table :reader root-table
               :initform (make-instance 'table))
   (current-table :accessor current-table :initform nil)))

(defun parse-toml-blocks (list)
  "Given a LIST of components (tables or key-value pairs), return an alist."
  (let ((context (make-instance 'parser-context)))
    (setf (current-table context) (root-table context))
    (mapc (lambda (toml-block) (parse-toml-block toml-block context)) list)
    (root-table context)))  

(defgeneric parse-toml-block (toml-block context))

(defmethod parse-toml-block ((toml-table toml-named-table) context)
  (loop with names = (toml-block:names toml-table)
        with length = (length names)
        with current-table = (root-table context)
        for name in names
        for i from 1
        for last-name-p = (= i length)
        for table = (get-child current-table name)
        if (null table)
          do (let ((table (make-instance 'table)))
               (when last-name-p
                 (setf (definition-context table) t)
                 (setf (current-table context) table))
               (set-child current-table name table)
               (setf current-table table))
        else
          do (case (type-of table)
               (table (if last-name-p
                          (progn (if (definition-context table)
                                     (error 'toml-redefine-table-error
                                            :names names)
                                     (setf (definition-context table) t))
                                 (setf (current-table context) table))
                          (setf current-table table)))
               (table-array (setf current-table (last-child table)))
               (t (error 'toml-redefine-table-error :names names)))))

(defmethod parse-toml-block ((toml-table toml-array-table) context)
  (loop with names = (toml-block:names toml-table)
        with length = (length names)
        with current-table = (root-table context)
        for name in names
        for i from 1
        for last-name-p = (= i length)
        for table = (get-child current-table name)
        if (null table)
          do (if last-name-p
                 ;; For last part of names, create table array.
                 (let ((table (make-instance 'table))
                       (table-array (make-instance 'table-array)))
                   (set-child current-table name table-array)
                   (append-child table-array table)
                   (setf (current-table context) table))
                 ;; For middle part of names, create normal table.
                 (let ((table (make-instance 'table)))
                   (set-child current-table name table)
                   (setf current-table table)))
        else
          do (case (type-of table)
               (table (if last-name-p
                          (error 'toml-redefine-table-error :names names)
                          (setf current-table table)))
               (table-array (if last-name-p
                                (let ((new-table (make-instance 'table)))
                                  (append-child table new-table)
                                  (setf (current-table context) new-table))
                                (setf current-table (last-child table))))
               (t (error 'toml-redefine-table-error :names names)))))

(defmethod parse-toml-block ((pair toml-key-value-pair) context)
  (let* ((current-table (current-table context))
         (table current-table)
         key-to-add value-to-add)
    ;; Parse keys.
    (loop with keys = (toml-block:keys pair)
          with length = (length keys)
          for key in keys
          for i from 1
          for last-name-p = (= i length)
          for value = (get-child table key)
          if (null value)
            do (if last-name-p
                   (setf key-to-add key)
                   (let ((new-table (make-instance
                                     'table
                                     :definition-context current-table)))
                     (set-child table key new-table)
                     (setf table new-table)))
          else
            do (if last-name-p
                   (error 'toml-redefine-property-error :names keys)
                   (case (type-of value)
                     (table (if (equal (definition-context value)
                                       current-table)
                                (setf table value)
                                (error 'toml-dotted-key-redefine-table-error
                                       :names keys)))
                     (inline-table (error 'toml-modify-inline-table-error
                                          :names keys))
                     (table-array (error 'toml-dotted-key-open-table-array-error
                                         :names keys))
                     (t (error 'toml-redefine-property-error
                               :names keys)))))

    (setf value-to-add
          (parse-pair-value (toml-block:value pair) context))

    ;; Parse value.
    (set-child table
               key-to-add
               value-to-add
               ;; (parse-pair-value (toml-block:value pair) context)
               )))

(defun parse-pair-value (value context)
  (cond
    ((typep value 'toml-block:toml-inline-table)
     (let ((inline-table (make-instance 'inline-table))
           (current-table (current-table context)))
       (setf (current-table context) inline-table)
       (parse-toml-block value context)
       (setf (current-table context) current-table)
       inline-table))
    ((and (listp value)
          (listp (cdr value)))
     (mapcar (lambda (v) (parse-pair-value v context)) value))
    (t value)))

(defmethod parse-toml-block ((toml-table toml-inline-table) context)
  (loop for pair in (toml-block:pairs toml-table)
        do (parse-toml-block pair context)))

(defun append-child (table-array table)
  "Append TABLE as a child to TABLE-ARRAY."
  (appendf (children table-array) (list table)))

(defun last-child (table-array)
  "Get the last child of TABLE-ARRAY."
  (first (last (children table-array))))

(defun set-child (table name value)
  "Set the child of TABLE specified by NAME to VALUE."
  (setf (gethash name (children table)) value))

(defun get-child (table name)
  "Get the child of TABLE specified by NAME."
  (gethash name (children table)))
