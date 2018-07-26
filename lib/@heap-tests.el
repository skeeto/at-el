;;; -*- lexical-binding: t; -*-

(require 'ert)
(require '@heap)
(require 'cl-lib)

(ert-deftest @heap-test ()
  (let ((h (@! @heap :new #'cdr)))
    (@! h :add '(d . 4))
    (@! h :add '(b . 2))
    (@! h :add '(c . 3))
    (@! h :add '(a . 1))
    (should (eq 'a (car (@! h :peek))))
    (should (equal '(a b c d) (mapcar #'car (@! h :to-list))))
    (should
     (equal '(a b c d)
            (cl-loop until (@! h :emptyp) collect (car (@! h :next)))))))
