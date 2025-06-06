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


(defclass fundamental-promise ()
  ()
  (:metaclass closer-mop:funcallable-standard-class))

(defclass promise (fundamental-promise)
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
   (%fullfilled
    :initarg :fullfilled
    :accessor fullfilled))
  (:metaclass closer-mop:funcallable-standard-class))

(defgeneric force (promise &key timeout loop)
  (:method ((promise t) &key timeout loop)
    (declare (ignore timeout loop))
    (values promise t)))

(defmethod force ((promise promise) &key timeout (loop t))
  (bind (((:accessors promise-success-p lock cvar result fullfilled) promise))
    (bt2:with-lock-held (lock)
      (if loop
          (iterate
            (until fullfilled)
            (bt2:condition-wait cvar lock :timeout timeout)
            (finally
             (if promise-success-p
                 (return-from force (values result fullfilled))
                 (error result))))
          (progn
            (unless fullfilled
              (bt2:condition-wait cvar lock :timeout timeout))
            (if fullfilled
                (if promise-success-p
                    (return-from force (values result fullfilled))
                    (error result))
                (values nil nil)))))))

(defgeneric fullfilledp (promise &optional timeout)
  (:method ((promise t) &optional timeout)
    (declare (ignore timeout))
    t))

(defmethod fullfilledp ((promise promise) &optional timeout)
  (bt2:with-lock-held ((lock promise) :timeout timeout)
    (handler-case (fullfilled promise)
      (bt2:timeout (e)
        (declare (ignore e))
        nil))))

(defgeneric fullfill! (promise &optional value)
  (:method ((promise t) &optional value)
    (or value promise)))

(defmethod initialize-instance :after ((obj fundamental-promise) &key &allow-other-keys)
  (closer-mop:set-funcallable-instance-function obj (curry #'fullfill! obj)))

(defmethod fullfill! ((promise promise) &optional (value nil result-bound-p))
  (bind (((:accessors lock cvar callback result fullfilled successp ) promise))
    (unwind-protect
         (progn
           (bt2:with-lock-held (lock)
             (when fullfilled
               (return-from fullfill! result))
             (handler-case
                 (setf fullfilled t
                       result (if result-bound-p value (funcall callback))
                       successp t)
               (error (s)
                 (setf result s)
                 (error s)))
             result))
      (bt2:condition-notify cvar))))

(defgeneric cancel! (promise &rest all &key condition timeout))

(errors:def canceled ()
  ())

(errors:def unable-to-cancel ()
  ())

(errors:def already-fullfilled (unable-to-cancel)
  ())

(defmethod cancel! ((promise promise) &rest all &key (condition (make-condition 'canceled)) (timeout nil))
  (declare (ignore all))
  (bind (((:accessors lock cvar result fullfilled canceled) promise))
    (handler-case
        (bt2:with-lock-held (lock :timeout timeout)
          (when fullfilled
            (errors:!!! already-fullfilled ("Can't cancel because promise is already fullfilled")))
          (setf result condition
                canceled t
                fullfilled t))
      (error (e)
        (errors:!!! unable-to-cancel ("Unable to cancel.") :cause e)))
    (bt2:condition-notify cvar))
  promise)

(defun make (callback)
  (make-instance 'promise
                 :callback callback
                 :result nil
                 :successp nil
                 :fullfilled nil))

(defmacro promise (&body body)
  `(make (lambda () ,@body)))

(defun find-fullfilled (promise &rest promises)
  (iterate
    (with all-promises = (cons promise promises))
    (for i from 0)
    (for promise in all-promises)
    (for (values v success) = (force promise :loop nil :timeout 0.1))
    (when success (return-from find-fullfilled (values v i)))))

(defun force-all (promises &key timeout (loop t))
  (map 'list
       (lambda (promise)
         (force promise :timeout timeout :loop loop))
       promises))
