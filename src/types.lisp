;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.redact)

(deftype string-list ()
  `(and list
        (satisfies string-list-p)))

(deftype string-alist ()
  `(and list
        (satisfies string-alist-p)))

(deftype string-key-alist ()
  `(and list
        (satisfies string-key-alist-p)))

(defun string-list-p (lst)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type list lst))
  (every #'stringp lst))

(defun string-alist-p (lst)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type list lst))
  (every
   #'(lambda (entry)
       (and (consp entry)
            (stringp (car entry))
            (stringp (cdr entry))))
   lst))

(defun string-key-alist-p (lst)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type list lst))
  (every
   #'(lambda (entry)
       (and (consp entry)
            (stringp (car entry))))
   lst))
