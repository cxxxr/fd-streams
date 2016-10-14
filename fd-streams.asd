(in-package :cl-user)
(defpackage :fd-streams-asd
  (:use :cl :asdf))
(in-package :fd-streams-asd)

(defsystem fd-streams
  :depends-on (:trivial-gray-streams
               :cffi
               :babel)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "fd-streams"))))
  :in-order-to ((test-op (test-op fd-streams-test))))
