;;; @-tests.el -*- lexical-binding: t; -*-

(require 'ert)
(defalias 'deftest 'ert-deftest)

(deftest @-inheritance ()
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

(deftest @-super ()
  (let* ((a (@extend :foo :a))
         (b (@extend a :foo :b)))
    (should (eq :b (@ b :foo)))
    (should (eq :a (@ b :foo :super t)))))

(deftest @-setf ()
  (let ((a (@extend :foo :before)))
    (should (eq :before (@ a :foo)))
    (setf (@ a :foo) :after)
    (should (eq :after (@ a :foo)))))

(deftest @-instance-of ()
  "Tests the @is function."
  (should (@is (@extend) @))
  (should (@is (@extend (@extend)) @))
  (should-not (@is @ (@extend)))
  (should-not (@is t @))
  (should-not (@is @ t)))

(deftest @-method ()
  "Tests method calls."
  (should
   (string=
    "Hi, Foo"
    (let ((foo (@extend :greet (lambda (@ name) (concat "Hi, " name)))))
      (@! foo :greet "Foo")))))

(deftest @-replace ()
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

;;; @-tests.el ends here
