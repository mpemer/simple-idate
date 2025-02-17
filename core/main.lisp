;;;; -----------------------------------------------------------------------
;;;; Filename: simple-idate.lisp
;;;; Author: Marcus Pemer
;;;; Email: marcus@pemer.com
;;;;
;;;; Description:
;;;; This file contains a collection of utility functions for working with
;;;; integer-based date representations in Lisp. The integer-based date is
;;;; stored in the YYYYMMDD format. Functions for converting between 
;;;; timestamps, parsing strings, performing date arithmetic, and 
;;;; comparisons are included.
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

(defpackage :simple-idate/core/main
  (:nicknames :simple-idate)
  (:use :common-lisp)
  (:import-from :parse-number :parse-integer)
  (:import-from :local-time
                :timestamp-year
                :timestamp-month
                :timestamp-day
                :timestamp> :timestamp>= :timestamp< :timestamp<= :timestamp=
                :timestamp+ :timestamp-
                :encode-timestamp :timestamp-to-unix)
  (:export :idate :date->idate :idate->date :idate->unix :idate-year :idate-month :idate-day
           :mm/dd/yyyy :yyyy-mm-dd :first-of-month :ints->idate
           :idate= :idate< :idate<= :idate> :idate>= :+1d :-1d :+1m
           :ints->idate))

(in-package :simple-idate)

;;; Converts year, month, and day integers into an integer-based date in YYYYMMDD format.
(defun ints->idate (y m d)
  "Convert separate year, month, and day integers into a single integer representation of the date."
  (+ (* 10000 y)
     (* 100 m)
     d))

;;; Converts a timestamp object into an integer-based date.
(defun date->idate (d)
  "Convert a timestamp object D to an integer-based date."
  (ints->idate (timestamp-year d)
               (timestamp-month d)
               (timestamp-day d)))

;;; Creates an integer-based date from year, month, and day values.
(defun idate (y m d)
  "Create an integer-based date from year Y, month M, and day D."
  (date->idate
   (encode-timestamp
    0 0 0 0 d m y)))

;;; Converts an integer-based date into a timestamp object.
(defun idate->date (idt)
  "Convert an integer-based date IDT back into a timestamp object."
  (multiple-value-bind (y x) (truncate idt 10000)
    (multiple-value-bind (m d) (truncate x 100)
      (encode-timestamp 0 0 0 0 d m y))))

;;; Converts an integer-based date into a Unix timestamp.
(defun idate->unix (idt)
  "Convert an integer-based date IDT into a Unix timestamp."
  (timestamp-to-unix (idate->date idt)))

;;; Extracts the year from an integer-based date.
(defun idate-year (idt)
  "Extract the year from an integer-based date IDT."
  (truncate idt 10000))

;;; Extracts the month from an integer-based date.
(defun idate-month (idt)
  "Extract the month from an integer-based date IDT."
  (multiple-value-bind (_ md) (truncate idt 10000)
    (declare (ignorable _))
    (truncate md 100)))

;;; Extracts the day from an integer-based date.
(defun idate-day (idt)
  "Extract the day from an integer-based date IDT."
  (multiple-value-bind (y x) (truncate idt 10000)
    (multiple-value-bind (_ d) (truncate x 100)
      (declare (ignore _))
      d)))

;;; Parses a date string in the MM/DD/YYYY format and converts it into an integer-based date.
(defun mm/dd/yyyy (s)
  "Parse a date string S in MM/DD/YYYY format and return an integer-based date."
  (destructuring-bind (m d y)
      (mapcar #'parse-integer
              (str:split #\/ s))
    (ints->idate y m d)))

;;; Parses a date string in the YYYY-MM-DD format and converts it into an integer-based date.
(defun yyyy-mm-dd (s)
  "Parse a date string S in YYYY-MM-DD format and return an integer-based date."
  (destructuring-bind (y m d)
      (mapcar #'parse-integer
              (str:split #\- s))
    (ints->idate y m d)))

;;; Macro to apply a timestamp comparison or operation to integer-based dates.
(defmacro idateop (op a b)
  `(funcall ,op (idate->date ,a) (idate->date ,b)))

;;; Compares two integer-based dates for equality.
(defun idate= (a b)
  "Check if two integer-based dates A and B are equal."
  (idateop #'timestamp= a b))

;;; Checks if one integer-based date is less than or equal to another.
(defun idate<= (a b)
  "Check if integer-based date A is less than or equal to B."
  (idateop #'timestamp<= a b))

;;; Checks if one integer-based date is less than another.
(defun idate< (a b)
  "Check if integer-based date A is less than B."
  (idateop #'timestamp< a b))

;;; Checks if one integer-based date is greater than another.
(defun idate> (a b)
  "Check if integer-based date A is greater than B."
  (idateop #'timestamp> a b))

;;; Checks if one integer-based date is greater than or equal to another.
(defun idate>= (a b)
  "Check if integer-based date A is greater than or equal to B."
  (idateop #'timestamp>= a b))

;;; Adds one day to an integer-based date.
(defun +1d (idt)
  "Return an integer-based date incremented by one day."
  (date->idate
   (timestamp+ (idate->date idt) 1 :day)))

;;; Subtracts one day from an integer-based date.
(defun -1d (idt)
  "Return an integer-based date decremented by one day."
  (date->idate
   (timestamp- (idate->date idt) 1 :day)))

;;; Adds one month to an integer-based date.
(defun +1m (idt)
  "Return an integer-based date incremented by one month."
  (date->idate
   (timestamp+ (idate->date idt) 1 :month)))

;;; Returns the first day of the month for the given integer-based date.
(defun first-of-month (idt)
  "Return an integer-based date representing the first day of the month for IDT."
  (idate (idate-year idt)
         (idate-month idt)
         1))

