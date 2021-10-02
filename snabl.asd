(asdf:defsystem snabl
  :name "snabl"
  :version "1"
  :maintainer "codr7"
  :author "codr7"
  :description "An embedded scripting language"
  :licence "MIT"
  :serial t
  :components ((:file "snabl")
	       (:file "utils")
	       (:file "order")
	       (:file "pos")
	       (:file "errors")
	       (:file "type")
	       (:file "val")
	       (:file "prim")
	       (:file "scope")
	       (:file "func")
	       (:file "libs")
	       (:file "proc")
	       (:file "frame")
	       (:file "vm")
	       (:file "forms")
	       (:file "ops")
	       (:file "stack")
	       (:file "parsers")
	       (:file "repl")
	       (:file "tests")))
