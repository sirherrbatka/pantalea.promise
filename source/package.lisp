(cl:defpackage #:pantalea.promise
  (:use #:common-lisp #:iterate)
  (:local-nicknames)
  (:import-from #:metabang.bind
                #:bind)
  (:import-from #:alexandria
                #:curry)
  (:export
   #:fullfill!
   #:fullfilledp
   #:find-fullfilled
   #:attach-on-success!
   #:attach-on-failure!
   #:canceled
   #:*promises*
   #:*value*
   #:*value-bound-p*
   #:force-all!
   #:force!
   #:cancel!
   #:promise
   #:make))
