(require 'ert)

(ert-deftest @-inheritance ()
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

(ert-deftest @-funcall ()
  (should
   (string=
    "Hi, Foo"
    (let ((foo (@extend :greet (lambda (@ name) (concat "Hi, " name)))))
      (@! foo :greet "Foo")))))
