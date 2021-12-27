(defpackage clop.toml-value-parser
  (:use #:cl)
  (:local-nicknames (#:time #:local-time))
  (:export #:parse-value))

(in-package clop.toml-value-parser)

(defgeneric parse-value (type value))

(defmethod parse-value ((type (eql :datetime)) value)
  "Return a timestamp."
  (time:parse-timestring (str:replace-first " " "T" value)))

(defmethod parse-value ((type (eql :datetime-local)) value)
  "Return a plist with keys (:year :month :day :hour :minute :second)."
  (let* ((delimeter (str:s-nth 10 value))
         (splits (str:split delimeter value)))
    (append (parse-value :date-local (car splits))
            (parse-value :time-local (cadr splits)))))

(defmethod parse-value ((type (eql :date-local)) value)
  "Return a plist with keys (:year :month :day)."
  (let* ((time:*default-timezone* time:+utc-zone+)
         (timestamp (time:parse-timestring value)))
    (list :year (time:timestamp-year timestamp)
          :month (time:timestamp-month timestamp)
          :day (time:timestamp-day timestamp))))

(defmethod parse-value ((type (eql :time-local)) value)
  "Return a plist with keys (:hour :minute :second)."
  (let* ((time:*default-timezone* time:+utc-zone+)
         (timestamp (time:parse-timestring value)))
    (list :hour (time:timestamp-hour timestamp)
          :minute (time:timestamp-minute timestamp)
          :second (time:timestamp-second timestamp)
          :microsecond (time:timestamp-microsecond timestamp))))

(defmethod parse-value (type value)
  value)
