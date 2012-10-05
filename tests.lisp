(cl:in-package :cl-user)

(defpackage :dollar-macro-test
  (:use :cl :dollar-macro))

(in-package :dollar-macro-test)

(defmacro do-tests ((name &optional show-progress-p) &body body)
  "Helper macro which repeatedly executes BODY until the code in body
calls the function DONE.  It is assumed that each invocation of BODY
will be the execution of one test which returns NIL in case of success
and list of string describing errors otherwise.

The macro prints a simple progress indicator \(one dots for ten tests)
to *STANDARD-OUTPUT* unless SHOW-PROGRESS-P is NIL and returns a true
value iff all tests succeeded.  Errors in BODY are caught and reported
\(and counted as failures)."
  `(let ((successp t)
         (testcount 1))
     (block test-block
       (flet ((done ()
                (return-from test-block successp)))
         (format t "~&Test: ~A~%" ,name)
         (loop
          (when (and ,show-progress-p (zerop (mod testcount 10)))
            (format t ".")
            (when (zerop (mod testcount 100))
              (terpri))
            (force-output))
          (let ((errors
                 (handler-case
                     (progn ,@body)
                   (error (msg)
                     (list (format nil "~&got an unexpected error: ~A" msg))))))
            (setq successp (and successp (null errors)))
            (when errors
              (format t "~&~4@A:~{~&   ~A~}~%" testcount errors))
            (incf testcount)))))
     successp))

(defvar *simple-tests* 
  '((equal (macroexpand-1 '($)) 'nil)
    (equal (macroexpand-1 '($ f a b c)) '(f a b c))
    (equal (macroexpand-1 '($ f $ g a b c)) '(f (g a b c)))
    (equal (macroexpand-1 '($ f $ g $ h a b c)) '(f (g (h a b c))))
    (equal (macroexpand-1 '($ f a $ g b $ h c)) '(f a (g b (h c))))
    (equal (flet ((f (&rest rest) rest)) (funcall ($ f 1 2 $) 3))
     '(1 2 3))
    (equal (flet ((f (x) (* 3 x))
                   (g (x) (* 5 x)))
             (let ((fun ($ f $ g $)))
               (loop :for i :from 0 to 10 :collect (funcall fun  i))))
     '(0 15 30 45 60 75 90 105 120 135 150))
    (equal (flet ((f (x y) (* 2 x y))
                   (g (x y) (* 3 x y))
                   (h (x y) (* 5 x y)))
             (let* ((a 7)
                    (b 11)
                    (c 13)
                    (fun ($ f a $ g b $ h c $)))
               (loop :for i :from 0 to 10 :collect (funcall fun  i))))
     '(0 30030 60060 90090 120120 150150 180180 210210 240240 270270 300300))
    (equal (loop :for i :from 0 :to 9 :collect ($ nth i $f #'list 1 2 3 4 5 6 7 8 9))
     '(1 2 3 4 5 6 7 8 9 nil))
    (equal (mapcar ($ string-downcase 
                      $ symbol-name $)
            '(a b c))
     '("a" "b" "c"))
    (equal (list
            ($ identity $@ '+ $f (unless (oddp 2) 'mapcar) '1+ '(1 2 3 4 5 6 7 8 9))
            ($ identity $@ '* $f (unless (oddp 2) 'mapcar) '1+ '(1 2 3 4 5 6 7 8 9)))
     '(54 3628800))
    (equal ($^ list 1 2 $ list 3 4 $ list 5 6) '(5 6 (3 4 (1 2)))))
  "Test cases to")

(defun simple-tests (&key verbose)
  (do-tests ((format nil "Simple tests")
             (not verbose))
    (mapc (lambda (x)
        (with-standard-io-syntax
           (if verbose 
               (print x)
               (format t ".")))
        (unless (funcall (first x) (eval (second x)) (eval (third x))) (error "~S" x)))
      *simple-tests*)
    (done)))

(defun run-all-tests (&key more-tests verbose)
  "Runs all tests for dollar-macro and returns a true value if all tests
succeeded.  VERBOSE is interpreted by the individual test suites.
MORE-TESTS can be a list of function designators designating
additional tests to run."
  (let ((successp t))
    (macrolet ((run-test-suite (&body body)
                 `(unless (progn ,@body)
                    (setq successp nil))))
      (run-test-suite (simple-tests :verbose verbose))
      (when more-tests
        (unless (listp more-tests)
          (setq more-tests (list more-tests))
          (dolist (test more-tests)
            (run-test-suite (funcall test :verbose verbose))))))
    (format t "~2&~:[Some tests failed~;All tests passed~]." successp)
    successp))