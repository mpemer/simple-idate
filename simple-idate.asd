;;;; simple-idate.asd

(defsystem "simple-idate"
  :version "0.1"
  :author "Marcus Pemer"
  :license "GPLv3"
  :depends-on ("local-time" "parse-number" "str")
  :components ((:file "package")
               (:file "simple-idate"))
  :description "Description of your TDA system."
  :long-description "Longer description of your TDA system."
  :maintainer "marcus@pemer.com"
  :homepage "https://your-project-homepage-url.com/")
