;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(defsystem "foo.lisp.lack-middleware-security-headers"
  :version "1.0.0"
  :author "John Newton"
  :license "Apache-2.0"
  :homepage "https://github.com/lisplizards/lack-middleware-security-headers"
  :bug-tracker "https://github.com/lisplizards/lack-middleware-security-headers/issues"
  :source-control (:git "https://github.com/lisplizards/lack-middleware-security-headers.git")
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "middleware" :depends-on ("package"))
                 (:file "package"))))
  :description "Lack middleware to add security-related response headers"
  :in-order-to ((test-op (test-op "foo.lisp.lack-middleware-security-headers/tests"))))

(defsystem "foo.lisp.lack-middleware-security-headers/tests"
  :author "John Newton"
  :license "Apache-2.0"
  :depends-on ("foo.lisp.lack-middleware-security-headers"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "middleware" :depends-on ("package"))
                 (:file "package"))))
  :description "Test system for foo.lisp.lack-middleware-security-headers"
  :perform (test-op (op c) (symbol-call :rove :run c)))
