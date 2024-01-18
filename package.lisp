(in-package :cl-user)

(defpackage :simple-idate
  (:use :common-lisp :rutil)
  (:import-from :parachute :define-test)
  (:import-from :parse-number :parse-integer)
  (:import-from :local-time
                :timestamp-year
                :timestamp-month
                :timestamp-day
                :timestamp> :timestamp>= :timestamp< :timestamp<= :timestamp=
                :timestamp+ :timestamp-
                :encode-timestamp :timestamp-to-unix)
  (:export :idate :date->idate :idate->date :idate->unix :idate-year :idate-month :idate-day
   :mm/dd/yyyy :yyyy-mm-dd :first-of-month
   :idate= :idate< :idate<= :idate> :idate>= :+1d :-1d :+1m))

;;(let ((pack (find-package :simple-date)))
;;  (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export sym))))
