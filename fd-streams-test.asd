(in-package :cl-user)
(defpackage :fd-streams-test-asd
  (:use :cl :asdf))
(in-package :fd-streams-test-asd)

(defsystem fd-streams-test
  :depends-on (:fd-streams
               :prove
               :cl-ppcre)
  :components ((:module "t"
                        :components
                        ((:test-file "test"))))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
