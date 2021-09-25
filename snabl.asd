(asdf:defsystem snabl
  :name "snabl"
  :version "1"
  :maintainer "codr7"
  :author "codr7"
  :description "An embedded concatenative language."
  :licence "MIT"
  :serial t
  :components ((:file "utils")
	       (:file "snabl")
	       (:file "pos")
	       (:file "val")
	       (:file "libs")
	       (:file "scope")
	       (:file "vm")
	       (:file "forms")
	       (:file "ops")
	       (:file "stack")
	       (:file "parsers")
	       (:file "repl")
	       (:file "tests")))
