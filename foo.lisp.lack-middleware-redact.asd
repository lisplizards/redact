;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(defsystem "foo.lisp.lack-middleware-redact"
  :version "1.0.0"
  :author "John Newton"
  :license "Apache-2.0"
  :homepage "https://github.com/lisplizards/redact"
  :bug-tracker "https://github.com/lisplizards/redact/issues"
  :source-control (:git "https://github.com/lisplizards/redact.git")
  :depends-on ("foo.lisp.redact")
  :components ((:module "src"
                :components
                ((:module "middleware"
                  :components
                  ((:file "main" :depends-on ("package" "types"))
                   (:file "types" :depends-on ("package"))
                   (:file "package"))))))
  :description "Lack middleware to configure a redaction policy for downstream middlewares"
  :in-order-to ((test-op (test-op "foo.lisp.lack-middleware-redact/tests"))))

(defsystem "foo.lisp.lack-middleware-redact/tests"
  :author "John Newton"
  :license "Apache-2.0"
  :depends-on ("foo.lisp.lack-middleware-redact"
               "rove")
  :components ((:module "tests"
                :components
                ((:module "middleware"
                  :components
                  ((:file "main" :depends-on ("package"))
                   (:file "package"))))))
  :description "Test system for foo.lisp.lack-middleware-redact"
  :perform (test-op (op c) (symbol-call :rove :run c)))
