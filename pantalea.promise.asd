(asdf:defsystem #:pantalea.promise
  :name "promise"
  :depends-on (#:bordeaux-threads
               #:pantalea.errors
               #:closer-mop
               #:iterate
               #:metabang-bind
               #:alexandria)
  :serial T
  :pathname "source"
  :components ((:file "package")
               (:file "code")))
