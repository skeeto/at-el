;;; @-tests.el -*- lexical-binding: t; -*-

(require '@)
(require 'ert)

(ert-deftest @-inheritance ()
  "Tests prototype chain walking."
  (should
   (string= "left"
            (let* ((@root (@extend :name "root"))
                   (@left (@extend @root :name "left"))
                   (@right (@extend @root :name "right"))
                   (@top (@extend @left @right)))
              (@ @top :name))))
  (should
   (string= "right"
            (let* ((@root (@extend :name "root"))
                   (@left (@extend @root))
                   (@right (@extend @root :name "right"))
                   (@top (@extend @left @right)))
              (@ @top :name))))
  (should
   (string= "root"
            (let* ((@root (@extend :name "root"))
                   (@left (@extend @root))
                   (@right (@extend @root))
                   (@top (@extend @left @right)))
              (@ @top :name)))))

(ert-deftest @-super ()
  (let* ((a (@extend :foo :a))
         (b (@extend a :foo :b)))
    (should (eq :b (@ b :foo)))
    (should (eq :a (@ b :foo :super 1)))))

(ert-deftest @-setf ()
  (let ((a (@extend :foo :before)))
    (should (eq :before (@ a :foo)))
    (setf (@ a :foo) :after)
    (should (eq :after (@ a :foo)))))

(ert-deftest @-instance-of ()
  "Tests the @is function."
  (should (@is (@extend) @))
  (should (@is (@extend (@extend)) @))
  (should-not (@is @ (@extend)))
  (should-not (@is t @))
  (should-not (@is @ t)))

(ert-deftest @-method ()
  "Tests method calls."
  (should
   (string=
    "Hi, Foo"
    (let ((foo (@extend :greet (lambda (_ name) (concat "Hi, " name)))))
      (@! foo :greet "Foo")))))

(ert-deftest @-replace ()
  "Tests the @: replacement walker."
  (should (equal (@--walk '(setf @:name 10) '(quote) #'@--replace)
                 '(setf (@ @@ :name) 10)))
  (should (equal (@--walk '(setf '@:name 10) '(quote) #'@--replace)
                 '(setf '@:name 10)))
  (should (eq :bar (with-@@ (@extend :foo :bar) @:foo)))
  (should (eq :bar
              (let* ((a (@extend :foo :bar))
                     (b (@extend a :foo :foo)))
                (with-@@ b
                  @^:foo)))))

(ert-deftest @-delete ()
  "Tests the key :delete method."
  (should-error
   (let ((foo (@extend :foo 1)))
     (@! foo :delete :foo)
     (@ foo :foo)))
  (should
   (eq :foo
       (let* ((quux (@extend :foo :foo))
	      (bar (@extend quux :foo 1)))
	 (@! bar :delete :foo)
	 (@ bar :foo))))
  (should
   (equal '(:foo :proto)
	  (let* ((foo (@extend :foo :foo :bar :bar)))
	    (@! foo :delete :bar)
	    (@! foo :keys))))
  ;; ensure we don't actually delete the :proto key
  (should
   (equal '(:proto nil :foo :foo)
	  (let* ((foo (@extend :foo :foo)))
	    (@! foo :delete :proto)
	    (aref foo 1)))))

;;; @-tests.el ends here
