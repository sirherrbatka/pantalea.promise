#|
Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2) Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#
(cl:in-package #:pantalea.promise)


(defparameter *promises* (list))

(defclass promise ()
  ((%lock
    :initarg :lock
    :initform (bt2:make-lock :name "PROMISE lock")
    :accessor lock)
   (%cvar
    :initarg :cvar
    :initform (bt2:make-condition-variable)
    :accessor cvar)
   (%canceled
    :initarg :canceld
    :initform nil
    :accessor canceled)
   (%callback
    :initarg :callback
    :accessor callback)
   (%result
    :initarg :result
    :accessor result)
   (%successp
    :initarg :successp
    :reader promise-success-p
    :accessor successp)
   (%failure-hooks
    :initform nil
    :accessor failure-hooks)
   (%success-hooks
    :initform nil
    :accessor success-hooks)
   (%fullfilled
    :initarg :fullfilled
    :accessor fullfilled))
  (:metaclass closer-mop:funcallable-standard-class))

(defgeneric force! (promise &key timeout loop)
  (:method ((promise t) &key timeout loop)
    (declare (ignore timeout loop))
    (values promise t)))

(defmethod force! ((promise promise) &key timeout (loop t))
  (bind (((:accessors successp lock cvar result fullfilled) promise))
    (bt2:with-lock-held (lock)
      (if loop
          (iterate
            (until fullfilled)
            (bt2:condition-wait cvar lock :timeout timeout)
            (finally
             (if successp
                 (return-from force! (values result fullfilled))
                 (signal result))))
          (progn
            (unless fullfilled
              (bt2:condition-wait cvar lock :timeout timeout))
            (if fullfilled
                (if successp
                    (return-from force! (values result fullfilled))
                    (signal result))
                (values nil nil)))))))

(defgeneric fullfilledp (promise)
  (:method ((promise t))
    t))

(defmethod fullfilledp ((promise promise))
  (bt2:with-lock-held ((lock promise))
    (fullfilled promise)))

(defgeneric fullfill! (promise))

(defmethod initialize-instance :after ((obj promise) &key &allow-other-keys)
  (check-type fn function)
  (closer-mop:set-funcallable-instance-function obj #'fullfill!))

(defmethod fullfill! ((promise promise))
  (bind (((:accessors lock cvar callback result fullfilled successp success-hooks failure-hooks) promise)
         (*promises* (cons promise *promises*)))
    (unwind-protect
         (bt2:with-lock-held (lock)
           (when fullfilled
             (return-from fullfill! result))
           (handler-case
               (setf fullfilled t
                     result (funcall callback)
                     successp t)
             (condition (s)
               (setf result s)
               (signal s)))
           result)
      (iterate
        (for hook in (bt2:with-lock-held (lock) (if successp success-hooks failure-hooks)))
        (ignore-errors (funcall hook)))
      (bt2:condition-notify cvar))))

(defgeneric cancel! (promise &optional condition))

(define-condition canceled (error)
  ())

(defmethod cancel! ((promise promise) &optional (condition (make-condition 'canceled)))
  (bind (((:accessors lock cvar result failure-hooks fullfilled canceled) promise))
    (bt2:with-lock-held (lock)
      (unless fullfilled
        (setf result condition
              canceled t
              fullfilled t)
        (iterate
          (for hook in failure-hooks)
          (ignore-errors (funcall hook result)))))))

(defun make (callback)
  (make-instance 'promise
                 :callback callback
                 :result nil
                 :successp nil
                 :fullfilled nil))

(defgeneric attach-on-success!* (promise &rest callbacks))

(defgeneric attach-on-failure!* (promise &rest callbacks))

(defmethod attach-on-success!* ((promise promise) &rest callbacks)
  (bind (((:accessors lock cvar result fullfilled success-hooks canceled successp) promise))
    (bt2:with-lock-held (lock)
      (if fullfilled
          (if successp
              (iterate
                (for callback in callbacks)
                (ignore-errors (funcall callback result)))
              nil)
          (iterate
            (for callback in callbacks)
            (push callback success-hooks)))
      promise)))

(defmethod attach-on-failure!* ((promise promise) &rest callbacks)
  (bind (((:accessors lock cvar result fullfilled failure-hooks canceled successp) promise))
    (bt2:with-lock-held (lock)
      (if fullfilled
          (if successp
              nil
              (iterate
                (for callback in callbacks)
                (ignore-errors (funcall callback result))))
          (iterate
            (for callback in callbacks)
            (push callback failure-hooks)))
      promise)))

(defmacro attach-on-success! (promise &body hooks)
  `(attach-on-success!* ,promise
    ,@(mapcar (lambda (body) `(lambda () ,@body))
              hooks)))

(defmacro attach-on-failure! (promise &body hooks)
  `(attach-on-failure!* ,promise
    ,@(mapcar (lambda (body) `(lambda () ,@body))
              hooks)))

(defmacro promise (&body body)
  `(make-promise (lambda () ,@body)))

(defun find-fullfilled (promise &rest promises)
  (iterate
    (with all-promises = (cons promise promises))
    (iterate
      (for i from 0)
      (for promise in all-promises)
      (for (values v success) = (force! promise :loop nil :timeout 0.1))
      (when success (return-from find-fullfilled (values v i))))))

(defun force-all! (promises &key timeout (loop t))
  (map 'list
       (lambda (promise)
         (force! promise :timeout timeout :loop loop))
       promises))
