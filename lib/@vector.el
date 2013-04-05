;;; @vector.el --- vector prototype written in @

(require '@)

(defvar @vector (@extend :vector [] :fill 0))

(def@ @vector :size ()
  "Return the number of elements in this vector."
  @:fill)

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
  (if (integerp n)
      (if (< n @:fill)
          (aref @:vector n)
        (signal 'args-out-of-range (list (subseq @:vector 0 @:fill) n)))
    (@^:get n)))

(def@ @vector :to-list ()
  "Return the contents of this vector as a list."
  (coerce (subseq @:vector 0 @:fill) 'list))

;;; @vector.el ends here
