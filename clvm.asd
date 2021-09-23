(asdf:defsystem clvm
  :name "clvm"
  :version "1"
  :maintainer "codr7"
  :author "codr7"
  :description "A VM toolkit"
  :licence "MIT"
  :serial t
  :components ((:file "clvm")
	       (:file "libs")
	       (:file "vm")
	       (:file "forms")
	       (:file "ops")
	       (:file "stack")
	       (:file "tests")))
