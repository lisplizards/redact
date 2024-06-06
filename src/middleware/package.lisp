;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-user)

(defpackage #:lack/middleware/redact
  (:use #:cl)
  (:export #:*lack-middleware-redact*))
