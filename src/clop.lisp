(defpackage clop
  (:use #:cl)
  (:local-nicknames (#:rules #:clop.rules)
                    (#:parser #:clop.toml-block-parser))
  (:export #:parse))

(in-package clop)

(defun parse (text &key (style :alist))
  (let* ((parsed (esrap:parse 'rules:toml text)))
    (serialize parsed style)))

(defgeneric serialize (table style))

(defmethod serialize ((table parser:table) (style (eql :jsown)))
  (cons :obj
        (loop with children = (parser:children table)
              for key being the hash-keys of children
              collect (cons key (serialize (gethash key children) style)))))

(defmethod serialize ((table parser:inline-table) (style (eql :jsown)))
  (cons :obj
        (loop with children = (parser:children table)
              for key being the hash-keys of children
              collect (cons key (serialize (gethash key children) style)))))

(defmethod serialize ((table parser:table-array) (style (eql :jsown)))
  (mapcar (lambda (it) (serialize it style))
          (parser:children table)))

(defmethod serialize ((thing t) (style (eql :jsown)))
  thing)

(defmethod jsown:to-json ((timestamp local-time:timestamp))
  (format nil "\"~a\"" (local-time:to-rfc3339-timestring timestamp)))

;; (let* ((result (parse (alexandria:read-file-into-string
;;                        #P"/home/sheep/temp/silver-brain/config.toml")
;;                       :style :jsown)))
;;   (format t "~a" result)
;;   (format t "~a" (jsown:to-json result)))
