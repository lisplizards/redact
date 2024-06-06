;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:lack/middleware/redact/tests)

(deftest redact-middleware
  (testing
   "signals a SIMPLE-ERROR when PARAMETERS is NIL"
   (flet ((app (env)
            (declare (ignore env))
            `(200
              (:content-type "text/plain"
               :content-length 13)
              ("Hello, World."))))
     (ok (signals (funcall lack/middleware/redact:*lack-middleware-redact*
                           #'app
                           :parameters ())
                  'simple-error))))

  (testing
   "signals a SIMPLE-ERROR when PARAMETERS is not given"
   (flet ((app (env)
            (declare (ignore env))
            `(200
              (:content-type "text/plain"
               :content-length 13)
              ("Hello, World."))))
     (ok (signals (funcall lack/middleware/redact:*lack-middleware-redact*
                           #'app)
                  'simple-error))))

  (testing
   "signals a TYPE-ERROR when PARAMETERS includes a non-string value"
   (flet ((app (env)
            (declare (ignore env))
            `(200
              (:content-type "text/plain"
               :content-length 13)
              ("Hello, World."))))
     (ok (signals (funcall lack/middleware/redact:*lack-middleware-redact*
                           #'app
                           :parameters '("token" :code))
                  'type-error))))

  (testing
   "binds FOO.LISP.REDACT:*REDACT* to a REDACT-CONFIG instance constructed from middleware parameters"
   (flet ((app (env)
            (declare (ignore env))
            `(200
              (:content-type "text/plain"
               :content-length 13
               :x-redact-config ,foo.lisp.redact:*redact*
              ("Hello, World.")))))
     (let* ((wrapped-app (funcall lack/middleware/redact:*lack-middleware-redact*
                                  #'app
                                  :parameters '("code" "token" "password")
                                  :headers '("x-token")
                                  :preserve-cookies '("_foo")))
            (response (funcall wrapped-app ()))
            (response-headers (second response))
            (redact-config (getf response-headers :x-redact-config)))
       (ok (eq 'foo.lisp.redact::redact-config (type-of redact-config)))
       (ok (functionp (foo.lisp.redact::redact-config-parameter-scanner redact-config)))
       (ok (equal '("x-token")
                  (foo.lisp.redact::redact-config-headers redact-config)))
       (ok (equal '("_foo")
                  (foo.lisp.redact::redact-config-preserve-cookies redact-config)))))))
