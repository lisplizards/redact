;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.redact)

(defvar *redact* nil
  "Special variable for containing an instance of struct REDACT-CONFIG.
Intended to be dynamically bound from middleware.")

(defparameter *authorization-headers*
  '("authorization" "proxy-authorization")
  "List of authorization header names that require special treatment when redacting headers.")

(defstruct redact-config "Configuration related to the scrubbing of sensitive values from the ENV."
           (parameter-scanner (lambda (str) (declare (ignore str))) :type function)
           (headers () :type string-list)
           (preserve-cookies () :type string-list))

(declaim (ftype (function (list &optional (or boolean stream)) (or null string)) print-env))
(defun print-env (env &optional (destination *standard-output*))
  "Prints ENV to DESTINATION, redacting all values and printing hash-table
entries. When DESTINATION is NIL, returns a string; otherwise, when
DESTINATION is a stream or T, returns NIL. Intended to be called from any
downstream middlewares that present the ENV to the user, such as in backtrace
or notification related middlewares. Does not modify ENV."
  (declare (type list env)
           (type (or boolean stream) destination))
  (format destination
          "~{~A: ~A~%~}"
          (loop for (key value) of-type (keyword t) on (redact env)  by #'cddr
                append (list key (if (hash-table-p value)
                                     (foo.lisp.print-hash:print-hash value destination)
                                     value)))))

(declaim (ftype (function (list) list) redact))
(defun redact (env)
  "Redacts the Clack ENV by returning a new list scrubbed of sensitive data.
Intended to be called from any downstream middlewares that present the ENV
to the user where calling PRINT-ENV is not suitable due to formatting
requirements. Encapsulates all redacting logic; called by PRINT-ENV."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type list env))
  (loop for (key value) of-type (keyword t) on env by #'cddr
        append (list key
                     (case key
                       (:query-string (and value (redact-query-string value)))
                       (:query-parameters (and value (redact-parameters value)))
                       (:body-parameters (and value (redact-parameters value)))
                       (:cookies (and value (redact-cookies value)))
                       (:headers (and value (redact-headers value)))
                       (t (if (hash-table-p value)
                              (redact-hash-table value)
                              value))))))

(declaim (ftype (function (string-key-alist) string-key-alist) redact-parameters))
(defun redact-parameters (alist)
  "Maps over ALIST, returning a new association list with any values
redacted where the key is a partial, case-insensitive match to the
configured parameters. The new association list references the cons
in ALIST for each non-matching entry. Called by function REDACT to
scrub body and query parameters."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type string-alist alist))
  (let ((parameter-scanner (redact-config-parameter-scanner *redact*)))
    (declare (type function parameter-scanner))
    (mapcar
     (lambda (pair)
       (declare (optimize (speed 3) (safety 0) (debug 0))
                (type cons pair))
       (destructuring-bind (key . value)
           pair
         (declare (ignore value)
                  (type string key)
                  (type t value))
         (if (ppcre:scan parameter-scanner key)
             (cons key "[REDACTED]")
             pair)))
     alist)))

(declaim (ftype (function (string) string) redact-query-string))
(defun redact-query-string (str)
  "Returns a new query string, redacting any values where the query
parameter name is a partial, case-insensitive match to the configured
parameters. Called by function REDACT."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type string str))
  (let ((parameter-scanner (redact-config-parameter-scanner *redact*)))
    (declare (type function parameter-scanner))
    (format nil "~{~A~^&~}"
            (mapcar
             (lambda (lst)
               (destructuring-bind (key value)
                   lst
                 (if (ppcre:scan parameter-scanner key)
                     (format nil "~A=[REDACTED]" key)
                     (format nil "~A=~A" key value))))
             (mapcar (lambda (kv)
                       (uiop:split-string kv :separator "="))
                     (uiop:split-string str :separator "&"))))))

(declaim (ftype (function (string-alist) string-alist) redact-cookies))
(defun redact-cookies (alist)
  "Maps over ALIST, returning a new association list with any values
redacted where the cookie name is not a member of slot PRESERVE-COOKIES
in special variable *REDACT*. Called by function REDACT."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type string-alist alist))
  (let ((preserve-cookies (redact-config-preserve-cookies *redact*)))
    (declare (type string-list preserve-cookies))
    (mapcar
     (lambda (pair)
       (destructuring-bind (key . value)
           pair
         (declare (ignore value)
                  (type string key)
                  (type t value))
         (if (member key preserve-cookies :test #'equalp)
             pair
             (cons key "[REDACTED]"))))
     alist)))

(declaim (ftype (function (hash-table) hash-table) redact-headers))
(defun redact-headers (hash-table)
  "Returns a new hash-table, copied from HASH-TABLE, with any values
redacted where the hash-table key is not a member of slot HEADERS in
special variable *REDACT*. Redacts the Authorization header by default.
Called by function REDACT."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type hash-table hash-table))
  (let ((redact-headers (redact-config-headers *redact*))
        (copy (make-hash-table :test #'equal)))
    (declare (type hash-table copy)
             (type string-list redact-headers))
    (maphash
     (lambda (key value)
       (declare (type string key value))
       (setf (gethash key copy)
             (if (member key *authorization-headers* :test #'equalp)
                 (let ((split (uiop:split-string value)))
                   (declare (type string-list split))
                   (if (> (length split) 1)
                       (format nil "~A [REDACTED]" (first split))
                       "[REDACTED]"))
                 (if (member key redact-headers :test #'equal)
                     "[REDACTED]"
                     value))))
     hash-table)
    copy))

(declaim (ftype (function (hash-table) hash-table) redact-hash-table))
(defun redact-hash-table (hash-table)
  "Returns a new hash-table, copied from HASH-TABLE, with any values
redacted where the stringified hash-table key is partial, case-insensitive
match to the configured parameters. Descends recursively when the value is
another hash-table. Called by function REDACT."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type hash-table hash-table))
  (let ((copy (make-hash-table :test #'equal
                               :size (hash-table-count hash-table)
                               :rehash-size 1
                               :rehash-threshold 0))
        (parameter-scanner (redact-config-parameter-scanner *redact*)))
    (declare (type hash-table copy)
             (type function parameter-scanner))
    (maphash
     (lambda (key value)
       (let ((string-key (typecase key
                           (string key)
                           (t (format nil "~A" key)))))
         (declare (type string string-key))
         (setf (gethash key copy)
               (if (hash-table-p value)
                   (redact-hash-table value)
                   (if (ppcre:scan parameter-scanner string-key)
                       "[REDACTED]"
                       value)))))
     hash-table)
    copy))
