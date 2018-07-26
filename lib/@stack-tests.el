;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require '@stack)

(ert-deftest @stack-test ()
  (let ((s (@! @stack :new)))
    (@! s :push 'a)
    (should (eq 'a (@! s :peek)))
    (@! s :push 'b)
    (@! s :push 'c)
    (should
     (equal '(c b a)
            (@! s :to-list)))
    (should (= 3 (@! s :size)))
    (should
     (equal '(c b a)
            (cl-loop until (@! s :emptyp) collect (@! s :pop))))
    (should (= 0 (@! s :size)))))
