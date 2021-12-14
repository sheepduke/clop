(defpackage clode.util
  (:use #:cl)
  (:local-nicknames (#:time #:local-time))
  (:export #:parse-local-date-time
           #:parse-local-date
           #:parse-local-time))

(in-package clode.util)

(defun parse-local-date-time (date-time-string)
  "Parse given DATE-TIME-STRING and return a plist."
  (let* ((delimeter (str:s-nth 10 date-time-string))
         (splits (str:split delimeter date-time-string)))
    (append (parse-local-date (car splits))
            (parse-local-time (cadr splits)))))

(defun parse-local-date (date-string)
  "Parse given DATE-STRING of format like 1925-01-23 and return a
plist as (:year YEAR :month MONTH :day DAY)."
  (let* ((time:*default-timezone* time:+utc-zone+)
         (timestamp (time:parse-timestring date-string)))
    (list :year (time:timestamp-year timestamp)
          :month (time:timestamp-month timestamp)
          :day (time:timestamp-day timestamp))))

(defun parse-local-time (time-string)
  (let* ((time:*default-timezone* time:+utc-zone+)
         (timestamp (time:parse-timestring time-string)))
    (list :hour (time:timestamp-hour timestamp)
          :minute (time:timestamp-minute timestamp)
          :second (time:timestamp-second timestamp)
          :microsecond (time:timestamp-microsecond timestamp))))
