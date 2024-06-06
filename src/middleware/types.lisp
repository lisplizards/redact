;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:lack/middleware/redact)

(deftype string-list ()
  `(and list
        (satisfies string-list-p)))

(defun string-list-p (lst)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type list lst))
  (every #'stringp lst))
