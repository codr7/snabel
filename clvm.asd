(asdf:defsystem clvm
  :name "clvm"
  :version "1"
  :maintainer "codr7"
  :author "codr7"
  :description "A VM toolkit"
  :licence "MIT"
  :serial t
  :components ((:file "utils")
	       (:file "clvm")
	       (:file "val")
	       (:file "libs")
	       (:file "scope")
	       (:file "vm")
	       (:file "forms")
	       (:file "ops")
	       (:file "stack")
	       (:file "parsers")
	       (:file "lila")
	       (:file "tests")))
