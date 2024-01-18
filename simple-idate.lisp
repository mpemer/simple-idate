;;;; -----------------------------------------------------------------------
;;;; Filename: simple-idate.lisp
;;;; Author: Marcus Pemer
;;;; Email: marcus@pemer.com
;;;;
;;;; Description:
;;;; This file contains a collection of utility functions for use in Lisp
;;;; applications. It includes functions for parsing numbers, manipulating
;;;; hash tables, rounding numbers, processing lists with function chains
;;;; and monadic transformations, and other general utility functions.
;;;;
;;;; Copyright (C) 2023 Marcus Pemer
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;; -----------------------------------------------------------------------
(in-package #:simple-idate)

;;;; -----------------------------------------------------------------------
(defun ints->idate (y m d)
  (+ (* 10000 y)
      (* 100 m)
      d))

;;;; -----------------------------------------------------------------------
(define-test ints->idate-test
  ;; Test with a standard date
  (parachute:is = (ints->idate 2023 3 14) 20230314
                "Converting 2023-03-14 should yield 20230314.")

  ;; Test with a date in January (checking month formatting)
  (parachute:is = (ints->idate 2023 1 1) 2023010
                "Converting 2023-01-01 should yield 20230101.")

  ;; Test with a date in December (checking month and day formatting)
  (parachute:is = (ints->idate 2023 12 31) 20231231
                "Converting 2023-12-31 should yield 20231231.")

  ;; Test with a leap year date
  (parachute:is = (ints->idate 2024 2 29) 20240229
                "Converting 2024-02-29 (leap year) should yield 20240229."))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun date->idate (d)
  (ints->idate (timestamp-year d)
               (timestamp-month d)
               (timestamp-day d)))

;;;; -----------------------------------------------------------------------
(define-test date->idate-test
  ;; Assuming the presence of a function to create a mock timestamp
  (let ((mock-timestamp (encode-timestamp 0 0 0 0 14 3 2023)))
    (parachute:is = (date->idate mock-timestamp) 20230314
                  "Converting mock timestamp of 2023-03-14
                   should yield 20230314.")))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun idate (y m d)
  (date->idate
   (encode-timestamp
    0 0 0 0 d m y)))

;;;; -----------------------------------------------------------------------
(define-test idate-test
  (parachute:is (= (idate 2023 3 14) 20230314)
                "Creating idate for 2023-03-14 should yield 20230314."))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun idate->date (idt)
  (multiple-value-bind (y x) (truncate idt 10000)
    (multiple-value-bind (m d) (truncate x 100)
      (encode-timestamp 0 0 0 0 d m y))))

;;;; -----------------------------------------------------------------------
(define-test idate->date-test
  ;; Test with a standard integer date
  (let ((date (idate->date 20230314)))
    ;; Assuming encode-timestamp returns a date object or similar structure
    (parachute:is = (date-year date) 2023
                  "Year component of 20230314 should be 2023.")
    (parachute:is = (date-month date) 3
                  "Month component of 20230314 should be 3.")
    (parachute:is = (date-day date) 14
                  "Day component of 20230314 should be 14."))

  ;; Test with another integer date
  (let ((date (idate->date 20211225)))
    (parachute:is = (date-year date) 2021
                  "Year component of 20211225 should be 2021.")
    (parachute:is = (date-month date) 12
                  "Month component of 20211225 should be 12.")
    (parachute:is = (date-day date) 25
                  "Day component of 20211225 should be 25.")))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun idate->unix (idt)
  (timestamp-to-unix (idate->date idt)))

;;;; -----------------------------------------------------------------------
(define-test idate->unix-test
  ;; Test with a known integer date and its expected Unix timestamp
  (let ((known-unix-time 1678766400))
    (parachute:is (= (idate->unix 20230314) known-unix-time)
                  "Converting integer date 20230314
                   should yield the correct Unix timestamp."))

  ;; Test with another known integer date and its Unix timestamp
  (let ((known-unix-time 1640408400))
    (parachute:is (= (idate->unix 20211225) known-unix-time)
                  "Converting integer date 20211225
                   should yield the correct Unix timestamp.")))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun idate-year (idt)
  (truncate idt 10000))

;;;; -----------------------------------------------------------------------
(define-test idate-year-test
  ;; Test with a standard integer date
  (parachute:is = (idate-year 20230314) 2023
                "Extracting the year from 20230314 should yield 2023.")

  ;; Test with another integer date
  (parachute:is = (idate-year 20211225) 2021
                "Extracting the year from 20211225 should yield 2021.")

  ;; Test with a year in the distant past
  (parachute:is = (idate-year 19000101) 1900
                "Extracting the year from 19000101 should yield 1900.")

  ;; Test with a year in the future
  (parachute:is = (idate-year 20501231) 2050
                "Extracting the year from 20501231 should yield 2050."))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun idate-month (idt)
  (multiple-value-bind (_ md) (truncate idt 10000)
    (declare (ignorable _))
    (truncate md 100)))

;;;; -----------------------------------------------------------------------
(define-test idate-month-test
  ;; Test with a standard integer date
  (parachute:is = (idate-month 20230314) 3
                "Extracting the month from 20230314 should yield 3.")

  ;; Test with another integer date
  (parachute:is = (idate-month 20211225) 12
                "Extracting the month from 20211225 should yield 12.")

  ;; Test with a date in January
  (parachute:is = (idate-month 20230101) 1
                "Extracting the month from 20230101 should yield 1.")

  ;; Test with a date in December
  (parachute:is = (idate-month 20231231) 12
                "Extracting the month from 20231231 should yield 12.")

  ;; Test with a date in February of a leap year
  (parachute:is = (idate-month 20240229) 2
                "Extracting the month from 20240229 (leap year)
                 should yield 2."))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun idate-day (idt)
  (with ((y x (truncate idt 10000))
         (m d (truncate x 100)))
    (declare (ignorable m))
    d))

;;;; -----------------------------------------------------------------------
(define-test idate-day-test
  ;; Test with a standard integer date
  (parachute:is = (idate-day 20230314) 14
                "Extracting the day from 20230314 should yield 14.")

  ;; Test with another integer date
  (parachute:is = (idate-day 20211225) 25
                "Extracting the day from 20211225 should yield 25.")

  ;; Test with a date at the beginning of a month
  (parachute:is = (idate-day 20230301) 1
                "Extracting the day from 20230301 should yield 1.")

  ;; Test with a date at the end of a month
  (parachute:is = (idate-day 20230131) 31
                "Extracting the day from 20230131 should yield 31.")

  ;; Test with a leap day
  (parachute:is = (idate-day 20240229) 29
                "Extracting the day from 20240229 (leap year)
                 should yield 29."))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun mm/dd/yyyy (s)
  (destructuring-bind (m d y)
      (mapcar #'parse-integer
              (str:split #\/ s))
    (ints->idate y m d)))

;;;; -----------------------------------------------------------------------
(define-test mm-dd-yyyy-test
  ;; Test with a standard date string
  (parachute:is = (mm/dd/yyyy "03/14/2023") 20230314
                "Parsing '03/14/2023' should yield 20230314.")

  ;; Test with another date string
  (parachute:is = (mm/dd/yyyy "12/25/2021") 20211225
                "Parsing '12/25/2021' should yield 20211225.")

  ;; Test with a single-digit day and month
  (parachute:is = (mm/dd/yyyy "1/1/2021") 20210101
                "Parsing '1/1/2021' should yield 20210101.")

  ;; Test with invalid date format
  (parachute:fail (mm/dd/yyyy "2023-03-14")
      "Parsing an incorrectly formatted date string
       should fail.")

  ;; Test with invalid date values
  (parachute:fail (mm/dd/yyyy "13/32/2021")
      "Parsing an invalid date should fail."))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun yyyy-mm-dd (s)
  (destructuring-bind (y m d)
      (mapcar #'parse-integer
              (str:split #\- s))
    (ints->idate y m d)))

;;;; -----------------------------------------------------------------------
(define-test yyyy-mm-dd-test
  ;; Test with a standard date string
  (parachute:is = (yyyy-mm-dd "2023-03-14") 20230314
                "Parsing '2023-03-14' should yield 20230314.")

  ;; Test with another date string
  (parachute:is = (yyyy-mm-dd "2021-12-25") 20211225
                "Parsing '2021-12-25' should yield 20211225.")

  ;; Test with a single digit day and month
  (parachute:is = (yyyy-mm-dd "2021-1-1") 20210101
                "Parsing '2021-1-1' should yield 20210101.")

  ;; Test with invalid date format
  (parachute:is null (yyyy-mm-dd "14/03/2023")
                "Parsing an incorrectly formatted date string
                 should return NIL.")

  ;; Test with invalid date values
  (parachute:is null (yyyy-mm-dd "2021-13-32")
                "Parsing an invalid date should return NIL."))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
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

;;;; -----------------------------------------------------------------------
(define-test idate-comparison-tests
  ;; Test idate= (equal)
  (parachute:true (idate= 20230314 20230314)
                  "Comparing the same dates should return T for idate=.")
  (parachute:false (idate= 20230314 20211225)
                   "Comparing different dates
                    should return NIL for idate=.")

  ;; Test idate<= (less than or equal)
  (parachute:true (idate<= 20211225 20230314)
                  "Earlier date should be less than or
                   equal to later date for idate<=.")
  (parachute:true (idate<= 20230314 20230314)
                "Same date should be less than or equal for idate<=.")

  ;; Test idate< (less than)
  (parachute:true (idate< 20211225 20230314)
                "Earlier date should be less than later date for idate<.")
  (parachute:false (idate< 20230314 20230314)
                    "Same date should not be less than for idate<.")

  ;; Test idate> (greater than)
  (parachute:true (idate> 20230314 20211225)
                  "Later date should be greater
                   than earlier date for idate>.")
  (parachute:false (idate> 20230314 20230314)
                    "Same date should not be greater than for idate>.")

  ;; Test idate>= (greater than or equal)
  (parachute:true (idate>= 20230314 20211225)
                  "Later date should be greater than or equal
                   to earlier date for idate>=.")
  (parachute:true (idate>= 20230314 20230314)
                  "Same date should be greater than
                   or equal for idate>=."))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun +1d (idt)
  (date->idate
   (timestamp+ (idate->date idt) 1 :day)))

;;;; -----------------------------------------------------------------------
(define-test plus-1d-test
  ;; Test adding one day to a standard integer date
  (parachute:is = (+1d 20230314) 20230315
                "Adding one day to 2023-03-14 should yield 2023-03-15.")

  ;; Test adding one day to the last day of a month
  (parachute:is = (+1d 20230131) 20230201
                "Adding one day to 2023-01-31 should yield 2023-02-01.")

  ;; Test adding one day to the last day of a year
  (parachute:is = (+1d 20231231) 20240101
                "Adding one day to 2023-12-31 should yield 2024-01-01.")

  ;; Test adding one day to a leap day
  (parachute:is = (+1d 20240229) 20240301
                "Adding one day to 2024-02-29 (leap year)
                 should yield 2024-03-01."))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun -1d (idt)
  (date->idate
   (timestamp- (idate->date idt) 1 :day)))

;;;; -----------------------------------------------------------------------
(define-test minus-1d-test
  ;; Test subtracting one day from a standard integer date
  (parachute:is = (-1d 20230315) 20230314
                "Subtracting one day from 2023-03-15
                 should yield 2023-03-14.")

  ;; Test subtracting one day from the first day of a month
  (parachute:is = (-1d 20230201) 20230131
                "Subtracting one day from 2023-02-01
                 should yield 2023-01-31.")

  ;; Test subtracting one day from the first day of a year
  (parachute:is = (-1d 20240101) 20231231
                "Subtracting one day from 2024-01-01
                 should yield 2023-12-31.")

  ;; Test subtracting one day from March 1st on a leap year
  (parachute:is = (-1d 20240301) 20240229
                "Subtracting one day from 2024-03-01 (leap year)
                 should yield 2024-02-29."))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun +1m (idt)
  (date->idate
   (timestamp+ (idate->date idt) 1 :month)))

;;;; -----------------------------------------------------------------------
(define-test plus-1m-test
  ;; Test adding one month to a standard integer date
  (parachute:is = (+1m 20230314) 20230414
                "Adding one month to 2023-03-14 should yield 2023-04-14.")

  ;; Test adding one month to a date in December
  (parachute:is = (+1m 20231214) 20240114
                "Adding one month to 2023-12-14 should yield 2024-01-14.")

  ;; Test adding one month to a date at the end of a month
  (parachute:is = (+1m 20230131) 20230228
                "Adding one month to 2023-01-31 should adjust
                 for the shorter month.")

  ;; Test adding one month to February 28th on a non-leap year
  (parachute:is = (+1m 20230228) 20230328
                "Adding one month to 2023-02-28 should yield 2023-03-28.")

  ;; Test adding one month to February 29th on a leap year
  (parachute:is = (+1m 20240229) 20240329
                "Adding one month to 2024-02-29 (leap year)
                 should yield 2024-03-29."))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun first-of-month (idt)
  (idate (idate-year idt)
         (idate-month idt)
         1))

;;;; -----------------------------------------------------------------------
(define-test first-of-month-test
  ;; Test getting the first day of the month for a standard integer date
  (parachute:is = (first-of-month 20230314) 20230301
                "The first day of the month for 2023-03-14
                 should be 2023-03-01.")

  ;; Test getting the first day of the month for a date in December
  (parachute:is = (first-of-month 20231214) 20231201
                "The first day of the month for 2023-12-14
                 should be 2023-12-01.")

  ;; Test getting the first day of the month for a date in January
  (parachute:is = (first-of-month 20230131) 20230101
                "The first day of the month for 2023-01-31
                 should be 2023-01-01.")

  ;; Test getting the first day of the month for a leap year
  (parachute:is = (first-of-month 20240229) 20240201
                "The first day of the month for 2024-02-29 (leap year)
                 should be 2024-02-01."))
;;;; -----------------------------------------------------------------------

