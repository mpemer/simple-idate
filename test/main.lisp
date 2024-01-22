(defpackage :simple-idate/test/main
  (:use :common-lisp :simple-idate/core/main)
  (:import-from :local-time :encode-timestamp :timestamp-year :timestamp-month :timestamp-day)
  (:import-from :parachute :define-test :is :fail))


(in-package :simple-idate/test/main)


(define-test ints->idate-test
  ;; Test with a standard date
  (is = 20230314 (ints->idate 2023 3 14)
      "Converting 2023-03-14 should yield 20230314.")

  ;; Test with a date in January (checking month formatting)
  (is = 20230101 (ints->idate 2023 1 1)
      "Converting 2023-01-01 should yield 20230101.")

  ;; Test with a date in December (checking month and day formatting)
  (is = 20231231 (ints->idate 2023 12 31)
      "Converting 2023-12-31 should yield 20231231.")

  ;; Test with a leap year date
  (is = 20240229 (ints->idate 2024 2 29)
      "Converting 2024-02-29 (leap year) should yield 20240229."))



(define-test date->idate-test
  ;; Assuming the presence of a function to create a mock timestamp
  (let ((mock-timestamp (encode-timestamp 0 0 0 0 14 3 2023)))
    (is = 20230314 (date->idate mock-timestamp)
        "Converting mock timestamp of 2023-03-14
                   should yield 20230314.")))



(define-test idate-test
  (is = 20230314 (idate 2023 3 14)
      "Creating idate for 2023-03-14 should yield 20230314."))



(define-test idate->date-test
  ;; Test with a standard integer date
  (let ((date (idate->date 20230314)))
    ;; Assuming encode-timestamp returns a date object or similar structure
    (is = 2023 (timestamp-year date)
        "Year component of 20230314 should be 2023.")
    (is = 3 (timestamp-month date)
        "Month component of 20230314 should be 3.")
    (is = 14 (timestamp-day date)
        "Day component of 20230314 should be 14."))

  ;; Test with another integer date
  (let ((date (idate->date 20211225)))
    (is = 2021 (timestamp-year date)
        "Year component of 20211225 should be 2021.")
    (is = 12 (timestamp-month date)
        "Month component of 20211225 should be 12.")
    (is = 25 (timestamp-day date)
        "Day component of 20211225 should be 25.")))



(define-test idate->unix-test
  ;; Test with a known integer date and its expected Unix timestamp
  (let ((known-unix-time 1678766400))
    (is = known-unix-time (idate->unix 20230314)
        "Converting integer date 20230314
                   should yield the correct Unix timestamp."))

  ;; Test with another known integer date and its Unix timestamp
  (let ((known-unix-time 1640408400))
    (is = known-unix-time (idate->unix 20211225)
        "Converting integer date 20211225
                   should yield the correct Unix timestamp.")))



(define-test idate-year-test
  ;; Test with a standard integer date
  (is = 2023 (idate-year 20230314)
      "Extracting the year from 20230314 should yield 2023.")

  ;; Test with another integer date
  (is = 2021 (idate-year 20211225)
      "Extracting the year from 20211225 should yield 2021.")

  ;; Test with a year in the distant past
  (is = 1900 (idate-year 19000101)
      "Extracting the year from 19000101 should yield 1900.")

  ;; Test with a year in the future
  (is = 2050 (idate-year 20501231)
      "Extracting the year from 20501231 should yield 2050."))



(define-test idate-month-test
  ;; Test with a standard integer date
  (is = 3 (idate-month 20230314)
      "Extracting the month from 20230314 should yield 3.")

  ;; Test with another integer date
  (is = 12 (idate-month 20211225)
      "Extracting the month from 20211225 should yield 12.")

  ;; Test with a date in January
  (is = 1 (idate-month 20230101)
      "Extracting the month from 20230101 should yield 1.")

  ;; Test with a date in December
  (is = 12 (idate-month 20231231)
      "Extracting the month from 20231231 should yield 12.")

  ;; Test with a date in February of a leap year
  (is = 2 (idate-month 20240229)
      "Extracting the month from 20240229 (leap year)
                 should yield 2."))



(define-test idate-day-test
  ;; Test with a standard integer date
  (is = 14 (idate-day 20230314)
      "Extracting the day from 20230314 should yield 14.")

  ;; Test with another integer date
  (is = 25 (idate-day 20211225)
      "Extracting the day from 20211225 should yield 25.")

  ;; Test with a date at the beginning of a month
  (is = 1 (idate-day 20230301)
      "Extracting the day from 20230301 should yield 1.")

  ;; Test with a date at the end of a month
  (is = 31 (idate-day 20230131)
      "Extracting the day from 20230131 should yield 31.")

  ;; Test with a leap day
  (is = 29 (idate-day 20240229)
      "Extracting the day from 20240229 (leap year)
                 should yield 29."))



(define-test mm-dd-yyyy-test
  ;; Test with a standard date string
  (is = 20230314 (mm/dd/yyyy "03/14/2023")
      "Parsing '03/14/2023' should yield 20230314.")

  ;; Test with another date string
  (is = 20211225 (mm/dd/yyyy "12/25/2021")
      "Parsing '12/25/2021' should yield 20211225.")

  ;; Test with a single-digit day and month
  (is = 20210101 (mm/dd/yyyy "1/1/2021")
      "Parsing '1/1/2021' should yield 20210101.")

  ;; Test with invalid date format
  (fail (mm/dd/yyyy "2023-03-14") error
        "Parsing an incorrectly formatted date string
       should fail.")

  (when nil
    ;; Test with invalid date values - tbd
    (fail (mm/dd/yyyy "13/32/2021") error
          "Parsing an invalid date should fail."))
  )


(define-test yyyy-mm-dd-test
  ;; Test with a standard date string
  (is = 20230314 (yyyy-mm-dd "2023-03-14")
      "Parsing '2023-03-14' should yield 20230314.")

  ;; Test with another date string
  (is = 20211225 (yyyy-mm-dd "2021-12-25")
      "Parsing '2021-12-25' should yield 20211225.")

  ;; Test with a single digit day and month
  (is = 20210101 (yyyy-mm-dd "2021-1-1")
      "Parsing '2021-1-1' should yield 20210101.")

  ;; Test with invalid date format
  (is null (yyyy-mm-dd "14/03/2023")
      "Parsing an incorrectly formatted date string
                 should return NIL.")

  ;; Test with invalid date values
  (is null (yyyy-mm-dd "2021-13-32")
      "Parsing an invalid date should return NIL."))



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



(define-test plus-1d-test
  ;; Test adding one day to a standard integer date
  (is = 20230315 (+1d 20230314)
      "Adding one day to 2023-03-14 should yield 2023-03-15.")

  ;; Test adding one day to the last day of a month
  (is = 20230201 (+1d 20230131)
      "Adding one day to 2023-01-31 should yield 2023-02-01.")

  ;; Test adding one day to the last day of a year
  (is = 20240101 (+1d 20231231)
      "Adding one day to 2023-12-31 should yield 2024-01-01.")

  ;; Test adding one day to a leap day
  (is = 20240301 (+1d 20240229)
      "Adding one day to 2024-02-29 (leap year)
                 should yield 2024-03-01."))



(define-test minus-1d-test
  ;; Test subtracting one day from a standard integer date
  (is = 20230314 (-1d 20230315)
      "Subtracting one day from 2023-03-15
                 should yield 2023-03-14.")

  ;; Test subtracting one day from the first day of a month
  (is = 20230131 (-1d 20230201)
      "Subtracting one day from 2023-02-01
                 should yield 2023-01-31.")

  ;; Test subtracting one day from the first day of a year
  (is = 20231231 (-1d 20240101)
      "Subtracting one day from 2024-01-01
                 should yield 2023-12-31.")

  ;; Test subtracting one day from March 1st on a leap year
  (is = 20240229 (-1d 20240301)
      "Subtracting one day from 2024-03-01 (leap year)
                 should yield 2024-02-29."))



(define-test plus-1m-test
  ;; Test adding one month to a standard integer date
  (is = 20230414 (+1m 20230314)
      "Adding one month to 2023-03-14 should yield 2023-04-14.")

  ;; Test adding one month to a date in December
  (is = 20240114 (+1m 20231214)
      "Adding one month to 2023-12-14 should yield 2024-01-14.")

  ;; Test adding one month to a date at the end of a month
  (is = 20230228 (+1m 20230131)
      "Adding one month to 2023-01-31 should adjust
                 for the shorter month.")

  ;; Test adding one month to February 28th on a non-leap year
  (is = 20230328 (+1m 20230228)
      "Adding one month to 2023-02-28 should yield 2023-03-28.")

  ;; Test adding one month to February 29th on a leap year
  (is = 20240329 (+1m 20240229)
      "Adding one month to 2024-02-29 (leap year)
                 should yield 2024-03-29."))



(define-test first-of-month-test
  ;; Test getting the first day of the month for a standard integer date
  (is = 20230301 (first-of-month 20230314)
      "The first day of the month for 2023-03-14
                 should be 2023-03-01.")

  ;; Test getting the first day of the month for a date in December
  (is = 20231201 (first-of-month 20231214)
      "The first day of the month for 2023-12-14
                 should be 2023-12-01.")

  ;; Test getting the first day of the month for a date in January
  (is = 20230101 (first-of-month 20230131)
      "The first day of the month for 2023-01-31
                 should be 2023-01-01.")

  ;; Test getting the first day of the month for a leap year
  (is = 20240201 (first-of-month 20240229)
      "The first day of the month for 2024-02-29 (leap year)
                 should be 2024-02-01."))
