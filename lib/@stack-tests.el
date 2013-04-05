(require 'ert)
(defalias 'deftest 'ert-deftest)

(deftest @stack-test ()
  (let ((s (@extend @stack)))
    (@! s :push 'a)
    (should (eq 'a (@! s :peek)))
    (@! s :push 'b)
    (@! s :push 'c)
    (should
     (equal '(c b a)
            (@! s :to-list)))
    (should
     (equal '(c b a)
            (loop until (@! s :emptyp) collect (@! s :pop))))))
