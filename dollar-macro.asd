(cl:in-package :cl-user)

(defpackage :dollar-macro-asd
  (:use :cl :asdf))

(in-package :dollar-macro-asd)

(defsystem :dollar-macro
  :version "0.0.1"
  :description "macro that implemet Haskell $"
  :licence "MIT"
  :components ((:file "dollar-macro")))

(defsystem :dollar-macro-test
  :depends-on (:dollar-macro)
  :serial t
  :components ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system :dollar-macro))))
  (operate 'load-op :dollar-macro-test)
  (funcall (intern (symbol-name :run-all-tests) (find-package :dollar-macro-test))))
