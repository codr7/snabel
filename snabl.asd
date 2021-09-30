(asdf:defsystem snabl
  :name "snabl"
  :version "1"
  :maintainer "codr7"
  :author "codr7"
  :description "An embedded scripting language"
  :licence "MIT"
  :serial t
  :components ((:file "utils")
	       (:file "snabl")
	       (:file "pos")
	       (:file "errors")
	       (:file "type")
	       (:file "val")
	       (:file "prim")
	       (:file "func")
	       (:file "libs")
	       (:file "proc")
	       (:file "scope")
	       (:file "vm")
	       (:file "forms")
	       (:file "ops")
	       (:file "stack")
	       (:file "parsers")
	       (:file "repl")
	       (:file "tests")))
