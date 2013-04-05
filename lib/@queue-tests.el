(require 'ert)
(defalias 'deftest 'ert-deftest)

(deftest @queue-test ()
  (let ((q (@extend @queue)))
    (@! q :enqueue 0)
    (@! q :enqueue 1)
    (@! q :dequeue)
    (@! q :enqueue 2)
    (@! q :enqueue 3)
    (should
     (equal '(1 2 3)
            (loop until (@! q :emptyp) collect (@! q :dequeue))))))

(deftest @queue-stack ()
  (let ((q (@extend @queue @stack)))
    (@! q :enqueue 'b)
    (should (eq 'b (@! q :peek)))
    (@! q :enqueue 'c)
    (@! q :push 'a)
    (should
     (equal '(a b c)
            (@! q :to-list)))))
