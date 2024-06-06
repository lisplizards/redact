;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:lack/middleware/redact)

(defparameter *lack-middleware-redact*
  (lambda (app &key parameters
                 (headers '("cookie" "set-cookie"))
                 (preserve-cookies '("lack.session")))
    (declare (type function app))
    (check-type parameters list)
    (dolist (parameter parameters)
      (check-type parameter string))
    (assert (not (null parameters))
            nil
            "Redact middleware requires PARAMETERS to be non-NIL")
    (check-type headers list)
    (dolist (header-name headers)
      (check-type header-name string))
    (check-type preserve-cookies list)
    (dolist (cookie-name preserve-cookies)
      (check-type cookie-name string))
    (let ((parameter-scanner (ppcre:create-scanner
                              (format nil "~{~A~^|~}" parameters)
                              :case-insensitive-mode t)))
      (declare (type function parameter-scanner))
      (lambda (env)
        (declare (optimize (speed 3) (safety 0) (debug 0))
                 (type list env)
                 (type string-list parameters headers preserve-cookies))
        (let ((foo.lisp.redact:*redact* (foo.lisp.redact::make-redact-config
                                         :parameter-scanner parameter-scanner
                                         :headers headers
                                         :preserve-cookies preserve-cookies)))
          (declare (type foo.lisp.redact::redact-config foo.lisp.redact:*redact*))
          (funcall app env)))))
  "Lack middleware to bind special variable FOO.LISP.REDACT:*REDACT* to an instance
of FOO.LISP.REDACT::REDACT-CONFIG constructed from middleware parameters.")
