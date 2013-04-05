;;; @vector.el --- vector prototype written in @

(require '@)

(defvar @vector (@extend :vector [] :fill 0)
  "A dynamically growing vector with constant-time element access.")

(def@ @vector :init (&rest elements)
  "Initialize vector with ELEMENTS."
  (@^:init)
  (setf @:vector (coerce elements 'vector)
        @:fill (length elements)))

(def@ @vector :size ()
  "Return the number of elements in this vector."
  @:fill)

(def@ @vector :emptyp ()
  "Return t if this vector is empty."
  (= @:fill 0))

(def@ @vector :grow (&optional (factor 2))
  "Increase the capacity of this vector by FACTOR."
  (prog1 @@
      (let* ((new-length (max 1 (ceiling (* factor (length @:vector)))))
             (vec (make-vector new-length nil)))
        (loop for element across @:vector
              for i upfrom 0
              do (setf (aref vec i) element))
        (setf @:vector vec))))

(def@ @vector :trim ()
  "Free any extra space claimed by this vector."
  (setf @:vector (subseq @:vector 0 @:fill)))

(def@ @vector :push (&rest elements)
  "Append ELEMENTS to the end of this vector."
  (prog1 @@
      (let ((count (length elements)))
        (while (< (- (length @:vector) @:fill) count)
          (@:grow)))
    (dolist (element elements)
      (setf (aref @:vector @:fill) element)
      (incf @:fill))))

(def@ @vector :pop ()
  "Remove the element from the end of this vector and return it."
  (when (> @:fill 0)
    (prog1 (aref @:vector (decf @:fill))
      (setf (aref @:vector @:fill) nil))))

(def@ @vector :get (n)
  "Dynamic getter: get Nth element from this vector."
  (if (not (integerp n))
      (@^:get n)
    (if (< n @:fill)
        (aref @:vector n)
      (signal 'args-out-of-range (list (subseq @:vector 0 @:fill) n)))))

(def@ @vector :set (n value)
  "If N is an integer, sets the index in the vector to VALUE."
  (if (not (integerp n))
      (@^:set n value)
    (while (>= n @:fill)
      (@:grow))
    (setf (aref @:vector n) value)))

(def@ @vector :shift ()
  "Remove element from the front of this vector (slow)."
  (unless (@:emptyp)
    (prog1 (@ @@ 0)
      (setf @:vector (subseq @:vector 1))
      (decf @:fill))))

(def@ @vector :unshift (&rest elements)
  "Add elements from to the front of this vector (slow), returning this."
  (prog1 @@
      (setf @:vector (concatenate 'vector elements @:vector))
    (incf @:fill (length elements))))

(def@ @vector :to-list ()
  "Return the contents of this vector as a list."
  (coerce (subseq @:vector 0 @:fill) 'list))

;;; @vector.el ends here
