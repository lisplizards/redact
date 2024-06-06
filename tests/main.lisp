;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.redact/tests)

(defparameter *format-regex* "~{~A~^|~}")

(deftest redact
    (testing
     "returns a new env list with redacted values for query-string, query-parameters, body-parameters, cookies, and headers"
     (let ((foo.lisp.redact:*redact* (foo.lisp.redact::make-redact-config
                                      :parameter-scanner (ppcre:create-scanner
                                                          (format nil *format-regex*
                                                                  '("code" "token"))
                                                          :case-insensitive-mode t)
                                      :headers '("x-access-token" "x-secret")
                                      :preserve-cookies '("_sid"))))
       (let* ((request-env `(:query-string "code=xyz&token=abc&foo=bar"
                             :cookies (("_sid" . "64bd76074733ba73f159b8a4c8c32b4f1a23c09f")
                                       ("_foo" . "foobar123"))
                             :query-parameters (("code" . "xyz")
                                                ("token" . "abc")
                                                ("foo" . "bar"))
                             :body-parameters (("quux" . "baaz")
                                               ("code" . "abc123")
                                               ("token" . "asdf"))
                             :headers ,(alexandria:alist-hash-table
                                        '(("authorization" . "Bearer 003e6d43f8ffd30b7f3505d0b")
                                          ("x-secret" . "abc123456")
                                          ("x-access-token" . "asdf123456")
                                          ("x-foo" . "bar"))
                                        :test #'equal)))
              (redacted (foo.lisp.redact:redact request-env)))
         (ok (equal "code=[REDACTED]&token=[REDACTED]&foo=bar"
                    (getf redacted :query-string)))
         (ok (equal '(("code" . "[REDACTED]")
                      ("token" . "[REDACTED]")
                      ("foo" . "bar"))
                    (getf redacted :query-parameters)))
         (ok (equal '(("_sid" . "64bd76074733ba73f159b8a4c8c32b4f1a23c09f")
                      ("_foo" . "[REDACTED]"))
                    (getf redacted :cookies)))
         (ok (equal '(("quux" . "baaz")
                      ("code" . "[REDACTED]")
                      ("token" . "[REDACTED]"))
                    (getf redacted :body-parameters)))
         (ok (equalp (alexandria:alist-hash-table
                      '(("authorization" . "Bearer [REDACTED]")
                        ("x-secret" . "[REDACTED]")
                        ("x-access-token" . "[REDACTED]")
                        ("x-foo" . "bar"))
                      :test #'equal)
                     (getf redacted :headers))))))

  (testing
     "returns a new env list with redacted values, redacting partial matches with case insensitivity"
     (let ((foo.lisp.redact:*redact* (foo.lisp.redact::make-redact-config
                                      :parameter-scanner (ppcre:create-scanner
                                                          (format nil *format-regex*
                                                                  '("code" "token"))
                                                          :case-insensitive-mode t)
                                      :headers '("x-access-token" "x-secret")
                                      :preserve-cookies '("_sid"))))
       (let* ((request-env `(:query-string "foo[Code]=xyz&foo[Token]=abc&foo=bar"
                             :cookies (("_SID" . "64bd76074733ba73f159b8a4c8c32b4f1a23c09f")
                                       ("_foo" . "foobar123"))
                             :query-parameters (("foo[CODE]" . "xyz")
                                                ("foo[TOKEN]" . "abc")
                                                ("foo[foo]" . "bar"))
                             :body-parameters (("quux" . "baaz")
                                               ("FOO[Code]" . "abc123")
                                               ("Token" . "asdf"))
                             :headers ,(alexandria:alist-hash-table
                                        '(("authorization" . "Bearer 003e6d43f8ffd30b7f3505d0b")
                                          ("x-secret" . "abc123456")
                                          ("x-access-token" . "asdf123456")
                                          ("x-foo" . "bar"))
                                        :test #'equal)))
              (redacted (foo.lisp.redact:redact request-env)))
         (ok (equal "foo[Code]=[REDACTED]&foo[Token]=[REDACTED]&foo=bar"
                    (getf redacted :query-string)))
         (ok (equal '(("foo[CODE]" . "[REDACTED]")
                      ("foo[TOKEN]" . "[REDACTED]")
                      ("foo[foo]" . "bar"))
                    (getf redacted :query-parameters)))
         (ok (equal '(("_SID" . "64bd76074733ba73f159b8a4c8c32b4f1a23c09f")
                      ("_foo" . "[REDACTED]"))
                    (getf redacted :cookies)))
         (ok (equal '(("quux" . "baaz")
                      ("FOO[Code]" . "[REDACTED]")
                      ("Token" . "[REDACTED]"))
                    (getf redacted :body-parameters)))
         (ok (equalp (alexandria:alist-hash-table
                      '(("authorization" . "Bearer [REDACTED]")
                        ("x-secret" . "[REDACTED]")
                        ("x-access-token" . "[REDACTED]")
                        ("x-foo" . "bar"))
                      :test #'equal)
                     (getf redacted :headers)))))))
