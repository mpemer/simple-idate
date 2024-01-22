;;;; simple-idate.asd

(defsystem "simple-idate"
  :class :package-inferred-system
  :version (:read-file-line "version.txt" :at 0)
  :author "Marcus Pemer <marcus@pemer.com>"
  :license "GPL-3.0"
  :depends-on ("parachute" "local-time" "parse-number" "str" "simple-idate/core/main")
  :in-order-to ((test-op (load-op "simple-idate/test/main")))
  :perform (test-op (o c) (symbol-call :parachute 'test 'simple-idate/test/main))
  :description "Integer-based (yyyymmdd) simple date library."
  :long-description "The Simple-IDate Library is a Common Lisp utility designed for handling integer-based date representations. It provides a simple yet effective way of converting between standard date formats and integer-based dates."
  :maintainer "marcus@pemer.com"
  :homepage "https://github.com/mpemer/simple-idate")
