;;; @heap.el --- binary heap prototype written in @ -*- lexical-binding: t; -*-

(require '@)
(require '@vector)
(require 'cl-lib)

(with-no-warnings
  (defvar @heap (@extend :vector nil :heap-key #'identity :heap-compare #'<)
    "A priority heap prototype with extendable key and compare functions."))

(def@ @heap :init (&optional key compare)
  (setf @:vector (@! @vector :new))
  (when key (setf @:heap-key key))
  (when compare (setf @:heap-compare key)))

(def@ @heap :emptyp ()
  "Return t if this heap is empty."
  (not (@ @:vector 0)))

(def@ @heap :peek ()
  "Return the next output element of the heap without removing it."
  (@ @:vector 0))

(def@ @heap :add (element)
  "Add ELEMENT to this heap with PRIORITY priority. Return this heap."
  (prog1 @@
    (let* ((i (@! @:vector :size))
           (parent (floor (1- i) 2))
           (key @:heap-key)
           (compare @:heap-compare))
      (@! @:vector :push element)
      (cl-flet ((compare (a b)
                  (funcall compare (funcall key (@ @:vector a))
                          (funcall key (@ @:vector b)))))
        (while (and (>= parent 0) (compare i parent))
          (@! @:vector :swap i parent)
          (setf i parent
                parent (floor (1- parent) 2)))))))

(def@ @heap :next ()
  "Remove and return the next element in the heap."
  (prog1 (@ @:vector 0)
    (let* ((replace (@! @:vector :pop)))
      (unless (@! @:vector :emptyp)
        (setf (@ @:vector 0) replace)
        (cl-loop with compare = @:heap-compare
                 with key = @:heap-key
                 for i = 0 then largest
                 for a = (+ 1 (* i 2)) and b = (+ 2 (* i 2))
                 for na = (@ @:vector a) and nb = (@ @:vector b)
                 for largest =
                 (let ((largest i))
                   (if (and na (funcall compare (funcall key na)
                                        (funcall key (@ @:vector largest))))
                       (setf largest a))
                   (if (and nb (funcall compare (funcall key nb)
                                        (funcall key (@ @:vector largest))))
                       (setf largest b))
                   largest)
                 while (not (= largest i))
                 do (@! @:vector :swap i largest))))))

(def@ @heap :clone ()
  "Make a shallow copy of this heap."
  (@extend @@ :vector (@! @:vector :clone)))

(def@ @heap :to-list ()
  "Return the elements of this heap in order as a list."
  (let ((clone (@:clone)))
    (cl-loop until (@! clone :emptyp) collect (@! clone :next))))

(provide '@heap)

;;; @heap.el ends here
