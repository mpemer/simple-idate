(in-package #:simple-idate)


(defun ints->idate (y m d)
  (+ (* 10000 y)
      (* 100 m)
      d))


(defun date->idate (d)
  (ints->idate (timestamp-year d)
               (timestamp-month d)
               (timestamp-day d)))

(defun idate (y m d)
  (date->idate
   (encode-timestamp
    0 0 0 0 d m y)))


(defun idate->date (idt)
  (with ((y x (truncate idt 10000))
         (m d (truncate x 100)))
    (encode-timestamp 0 0 0 0 d m y)))


(defun idate->unix (idt)
  (timestamp-to-unix (idate->date idt)))


(defun idate-year (idt)
  (truncate idt 10000))


(defun idate-month (idt)
  (multiple-value-bind (_ md) (truncate idt 10000)
    (declare (ignorable _))
    (truncate md 100)))


(defun idate-day (idt)
  (with ((y x (truncate idt 10000))
         (m d (truncate x 100)))
    (declare (ignorable m))
    d))


(defun mm/dd/yyyy (s)
  (destructuring-bind (m d y)
      (mapcar #'parse-integer
              (str:split #\/ s))
    (ints->idate y m d)))


(defun yyyy-mm-dd (s)
  (destructuring-bind (y m d)
      (mapcar #'parse-integer
              (str:split #\- s))
    (ints->idate y m d)))


;;(defun idateop (op a b)
;;  (funcall op (idate->date a) (idate->date b)))

(defmacro idateop (op a b)
  `(funcall ,op (idate->date ,a) (idate->date ,b)))





(defun idate= (a b)
  (idateop #'timestamp= a b))


(defun idate<= (a b)
  (idateop #'timestamp<= a b))


(defun idate< (a b)
  (idateop #'timestamp< a b))


(defun idate> (a b)
  (idateop #'timestamp> a b))


(defun idate>= (a b)
  (idateop #'timestamp>= a b))


(defun +1d (idt)
  (date->idate
   (timestamp+ (idate->date idt) 1 :day)))


(defun -1d (idt)
  (date->idate
   (timestamp- (idate->date idt) 1 :day)))


(defun +1m (idt)
  (date->idate
   (timestamp+ (idate->date idt) 1 :month)))


(defun first-of-month (idt)
  (idate (idate-year idt)
         (idate-month idt)
         1))

