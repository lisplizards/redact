;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-user)

(defpackage #:foo.lisp.redact
  (:use #:cl)
  (:export #:*redact*
           #:print-env
           #:redact))

(defpackage #:foo.lisp.redact/backtrace
  (:use #:cl)
  (:export #:print-error
           #:print-error-to-stream))
