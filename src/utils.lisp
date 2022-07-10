(defpackage utils
  (:use :cl)
  (:export :when-opt
           :extract-elements
           :aif
           :uuid->name
           :it))
(in-package :utils)

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro when-opt ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun extract-elements (json key)
  (mapcar (lambda (x) (when (hash-table-p x)
                       (gethash key x)))
          json))

(defun uuid->name (uuid)
  (let ((json (dex:get (format nil "https://api.mojang.com/user/profiles/~a/names" uuid))))
    (when (stringp json)
      (gethash "name" (car (last (yason:parse json)))))))
