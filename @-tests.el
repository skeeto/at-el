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

(deftest @-instance-of ()
  "Tests the @of function."
  (should (@of (@extend) @))
  (should (@of (@extend (@extend)) @))
  (should-not (@of @ (@extend)))
  (should-not (@of t @))
  (should-not (@of @ t)))

(deftest @-method ()
  "Tests method calls."
  (should
   (string=
    "Hi, Foo"
    (let ((foo (@extend :greet (lambda (@ name) (concat "Hi, " name)))))
      (@! foo :greet "Foo")))))

(deftest @-replace ()
  "Tests the @: replacement walker."
  (should (equal (@--walk '(setf @:name 10))
                 '(setf (@ @@ :name) 10)))
  (should (equal (@--walk '(setf '@:name 10))
                 '(setf '@:name 10))))
