;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.redact/backtrace)

(defparameter *middleware-scanner*
  (ppcre:create-scanner
   "^(\\d+): .*(MIDDLEWARE|BACKTRACE|ENV).*$"))

(defun print-error (condition env destination print-env print-backtrace &optional (redact-backtrace t))
  (declare (type error condition)
           (type list env)
           (type (or stream pathname) destination)
           (type boolean print-env)
           (type boolean print-backtrace)
           (type boolean redact-backtrace))
  (etypecase destination
    (stream (print-error-to-stream condition env destination print-env print-backtrace redact-backtrace))
    (pathname (with-open-file (stream destination
                                      :external-format :utf-8
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
                (print-error-to-stream condition env stream print-env print-backtrace redact-backtrace)))))

(defun print-error-to-stream (condition env stream print-env print-backtrace &optional (redact-backtrace t))
  (declare (type error condition)
           (type list env)
           (type stream stream)
           (type boolean print-env)
           (type boolean print-backtrace)
           (type boolean redact-backtrace))
  (when print-env
    (format stream "~%ENV:~%---~%~A~%" (foo.lisp.redact:print-env env nil)))
  (when print-backtrace
    (if redact-backtrace
        (let* ((backtrace-string (trivial-backtrace:print-backtrace condition :output nil))
               (lines (uiop:split-string backtrace-string :separator (list #\Newline))))
          (dolist (line lines)
            (declare (type string line))
            (multiple-value-bind (start-match finish-match register-match-starts register-match-finishes)
                (ppcre:scan *middleware-scanner* line)
              (declare (ignore finish-match))
              (if start-match
                  (let ((line-num-string (subseq line
                                                 (aref register-match-starts 0)
                                                 (aref register-match-finishes 0))))
                    (declare (type string line-num-string))
                    (format stream "~A: [REDACTED]~%" line-num-string))
                  (format stream "~A~%" line)))))
        (trivial-backtrace:print-backtrace condition :output stream))))
