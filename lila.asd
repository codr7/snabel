(asdf:defsystem lila
  :name "lila"
  :version "1"
  :maintainer "codr7"
  :author "codr7"
  :description "An embedded, concatenative, interpreted language."
  :licence "MIT"
  :serial t
  :components ((:file "utils")
	       (:file "pos")
	       (:file "lila")
	       (:file "val")
	       (:file "lib")
	       (:file "libs")
	       (:file "scope")
	       (:file "vm")
	       (:file "forms")
	       (:file "ops")
	       (:file "stack")
	       (:file "parsers")
	       (:file "repl")
	       (:file "tests")))
