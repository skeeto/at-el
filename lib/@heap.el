;;; @heap.el --- binary heap prototype written in @

(require '@)

(defvar @heap (@extend :vector nil)
  "A min-heap prototype.")

(def@ @heap :init ()
  (setf @:vector (@! @vector :new)))

(def@ @heap :emptyp ()
  "Return t if this heap is empty."
  (not (@ @:vector 0)))

(def@ @heap :peek ()
  "Return the next output element of the heap without removing it."
  (car (@ @:vector 0)))

(def@ @heap :add (element priority)
  "Add ELEMENT to this heap with PRIORITY priority. Return this heap."
  (prog1 @@
    (let* ((i (@! @:vector :size))
           (parent (floor (1- i) 2)))
      (@! @:vector :push (cons element priority))
      (flet ((priority (p) (cdr (@ @:vector p))))
        (while (and (>= parent 0) (< priority (priority parent)))
          (@! @:vector :swap i parent)
          (setf i parent
                parent (floor (1- parent) 2)))))))

(def@ @heap :next ()
  "Remove and return the next element in the heap."
  (prog1 (car (@ @:vector 0))
    (let* ((replace (@! @:vector :pop))
           (priority (cdr replace)))
      (unless (@! @:vector :emptyp)
        (setf (@ @:vector 0) replace)
        (loop for i = 0 then largest
              for a = (+ 1 (* i 2)) and b = (+ 2 (* i 2))
              for na = (@ @:vector a) and nb = (@ @:vector b)
              for largest = (cond ((and na (< (cdr na) priority)) a)
                                  ((and nb (< (cdr nb) priority)) b)
                                  (i))
              while (not (= largest i))
              do (@! @:vector :swap i largest))))))

;; Local Variables:
;; lexical-binding: t
;; End:

(provide '@heap)

;;; @heap.el ends here
