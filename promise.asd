(asdf:defsystem #:promise
  :name "promise"
  :depends-on (#:bordeaux-threads #:closer-mop #:iterate #:metabang-bind)
  :serial T
  :pathname "source"
  :components ((:file "package")
               (:file "code")))
