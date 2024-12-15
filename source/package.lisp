(cl:defpackage #:pantalea.promise
  (:use #:common-lisp #:iterate #:metabang-bind)
  (:local-nicknames)
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
   #:force-all!
   #:force!
   #:cancel!
   #:promise
   #:make))
