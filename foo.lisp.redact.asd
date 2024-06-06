;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(defsystem "foo.lisp.redact"
  :version "1.0.0"
  :author "John Newton"
  :license "Apache-2.0"
  :homepage "https://github.com/lisplizards/redact"
  :bug-tracker "https://github.com/lisplizards/redact/issues"
  :source-control (:git "https://github.com/lisplizards/redact.git")
  :depends-on ("cl-ppcre"
               "foo.lisp.print-hash"
               "trivial-backtrace")
  :components ((:module "src"
                :components
                ((:file "backtrace" :depends-on ("package" "main"))
                 (:file "main" :depends-on ("package" "types"))
                 (:file "types" :depends-on ("package"))
                 (:file "package"))))
  :description "Redact the Clack environment to avoid leakage of PII and other sensitive data"
  :in-order-to ((test-op (test-op "foo.lisp.redact/tests"))))

(defsystem "foo.lisp.redact/tests"
  :author "John Newton"
  :license "Apache-2.0"
  :depends-on ("alexandria"
               "foo.lisp.redact"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main" :depends-on ("package"))
                 (:file "package"))))
  :description "Test system for foo.lisp.redact"
  :perform (test-op (op c) (symbol-call :rove :run c)))
