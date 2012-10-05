(cl:in-package :cl-user)

(defpackage #:dollar-macro-internal
  (:use :cl))

(in-package #:dollar-macro-internal)

(defvar *$separator* '#:$)
(defvar *$macroname* '#:$)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun $intern (package &rest params)
    (let ((*package* (find-package package)))
      (intern (apply #'format nil params) package))))

(defun is$ (x)
     (and (symbolp x)
          (string-equal x *$separator*)))

(defun startwith$ (x)
               (and (symbolp x)
                    (string-equal x *$separator* :end1 1)))

(defun $expander (package wholebody)
  (let ((partial :done)
        args)
    (labels ((expander (body)
               (cond
                 ((consp body)
                  (let ((ret (expander (rest body))))
                    (cond 
                      ((and (is$ (first body))
                            (endp (rest body))
                            (setq partial :not-processed)
                            '(#1= ())))
                      ((or (when (is$ (first body))
                             (let ((func (funcall (intern (symbol-name :dollar-macro-dispatcher) package)
                                                  t *$separator*)))
                               (when func
                                 (setf ret (cons func ret))))
                             t)
                           (and (startwith$ (first body))
                                (let ((func (funcall 
                                             (intern (symbol-name :dollar-macro-dispatcher) package)
                                             (intern (subseq (symbol-name (first body)) 1) :keyword)
                                             *$separator*)))
                                  (unless func 
                                    (error "Not supported ~A in ~S"
                                           (first body)
                                           wholebody))
                                  (setf ret (cons func ret)))))
                       (cond 
                         ((eql partial :not-processed)
                          (setq partial :lambda
                                args  (gensym (symbol-name :args)))
                          `((apply ,@(cons `(function ,(first ret)) 
                                           (rest (butlast ret))) ,args)))
                         (t (list ret))))
                      (t (cons (first body)
                               ret)))))
                 (t body))))
      (let ((ret (first (expander (cons *$macroname* wholebody))))
            (symbol (make-symbol (with-standard-io-syntax
                                   (format nil "~A" (cons *$macroname* wholebody))))))
        (if (eql partial :lambda)
            `(flet ((,symbol (&rest ,args)
                      ,ret))
               #',symbol)
            ret)))))

(defmacro define-$macro ($ &key
                           (internal $)
                           (external nil)
                           inverse
                           package)
  (let ((varsymbol ($intern package "*~A*" internal))
        (body (gensym "BODY"))
        (result (gensym "RESULT"))
        (tmp (gensym "TMP"))
        (i (gensym "I")))
    `(let ((*package* ,(find-package package)))
       (defvar ,varsymbol ,external)
       ,@(loop 
            :for symbol-name :in (cons "" (mapcar (lambda (x) 
                                                    (symbol-name (car x)))
                                                  (ignore-errors
                                                    (symbol-value external))))
            :for symbol := ($intern package "~A~A" $ symbol-name)
          
            :collect `(defmacro ,symbol (&body ,body)
                        (let ((*$separator* ',$)
                              (*$macroname* ',symbol))
                          ($expander ,package ,body))))
       ,@(when 
          inverse 
          `((defmacro ,($intern package
                                (format nil "~A~A" 
                                        (symbol-name $)
                                        (symbol-name inverse))) (&body ,body)
              (let ((*$separator* ',$)
                    (*$macroname* ',$))
                (loop 
                   :with ,result := ()
                   :with ,tmp := (list *$separator*)
                   :for ,i :in ,body
                   :do (if (and (symbolp ,i)
                                (startwith$ ,i))
                           (progn
                             (push (reverse ,tmp) ,result)
                             (setq ,tmp (list ($intern ,package (symbol-name ,i)))))
                           (push ,i ,tmp))
                   :finally (return ($expander ,package
                                     (cdr (apply #'append 
                                                 (if (is$ (first ,tmp))
                                                     (append ,result (list ,tmp))
                                                     (cons (reverse ,tmp) ,result)))))))))))
       (export ',($intern package "~A" :dollar-macro-dispatcher))
       (defun ,($intern package "~A" :dollar-macro-dispatcher)
           (&optional symbol (var *$separator*))
         (let* ((symbol% (intern (symbol-name symbol) :keyword))
                (var ($intern ,package "*~A*" var)))
           (if symbol
               (cdr (assoc symbol% (symbol-value var)))
               (copy-list (symbol-value var)))))
       (defun (setf ,($intern package "~A" :dollar-macro-dispatcher))
           (function symbol &optional (var *$separator*))
         (let* ((symbol (intern (symbol-name symbol) :keyword))
                (var ($intern ,package "*~A*" var))
                (val (symbol-value var)))
           (cond
             ((null function)
              (setf val (remove symbol val :key #'first)))
             ((assoc symbol val)
              (setf (cdr (assoc symbol val)) function))
             (t (setf val (acons symbol function val))))
           (setf (symbol-value var) (sort val #'string< :key #'first)))
         symbol)
       (export ',($intern package "~A" :dollar-macro-exporter))
       (defun ,($intern package "~A" :dollar-macro-exporter)
           (&key (degree :medium)
            (symbol :$))
         (let ((*package* (find-package ,package)))
           ;; unextern all the related symbols
           (loop 
              :with char := (aref (symbol-name symbol) 0)
              :for symbol :being :each external-symbols :in ,package
              :when (find char (symbol-name symbol))
              :do (unexport symbol))
           (flet (($ (x)
                     (export ($intern ,package
                                      (if (> (length x) 1)
                                          x
                                          (format nil "~A~A" 
                                                  (symbol-name symbol)
                                                  x)))
                             ,package)))
             ;;minimal
             ($ "")
             (case degree
               (:all
                (loop 
                   :with char := (aref (symbol-name :$) 0)
                   :for symbol :being :each symbols :in ,package
                   :when (find char (symbol-name symbol))
                   :do (export symbol)))
               (:small
                #1=(progn 
                     ($ "^")))
               (:medium
                #2=(progn 
                     #1#
                     ($ (format nil "*~A*"symbol))))
               (:large
                (progn
                  #2#
                  ;; not sure yes what should be included in :large.
                  )))))))))

(defpackage :dollar-macro
  (:use))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-$macro :$ 
      :external nil
      :inverse :^
      :package :dollar-macro))

(cl:setf (dollar-macro::dollar-macro-dispatcher :@) 'cl:apply)
(cl:setf (dollar-macro::dollar-macro-dispatcher :f) 'cl:funcall)
(dollar-macro::dollar-macro-exporter :degree :medium)
