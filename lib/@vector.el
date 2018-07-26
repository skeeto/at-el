;;; @vector.el --- vector prototype written in @ -*- lexical-binding: t; -*-

(require '@)
(require 'cl-lib)

(with-no-warnings
  (defvar @vector (@extend :vector [] :fill 0 :infinite t :vector-default nil)
    "A dynamically growing vector with constant-time element
access. If :infinite is t then array access is unbounded to the
right (i.e. all non-negative accesses are valid)."))

(def@ @vector :init (&rest elements)
  "Initialize vector with ELEMENTS."
  (@^:init)
  (setf @:vector (cl-coerce elements 'vector)
        @:fill (length elements)))

(def@ @vector :size ()
  "Return the number of elements in this vector."
  @:fill)

(def@ @vector :capacity ()
  "Return the current capacity of this vector."
  (length @:vector))

(def@ @vector :emptyp ()
  "Return t if this vector is empty."
  (= @:fill 0))

(def@ @vector :grow (&optional (factor 2))
  "Increase the capacity of this vector by FACTOR."
  (prog1 @@
      (let* ((new-length (max 1 (ceiling (* factor (length @:vector)))))
             (vec (make-vector new-length @:vector-default)))
        (cl-loop for element across @:vector
                 for i upfrom 0
                 do (setf (aref vec i) element))
        (setf @:vector vec))))

(def@ @vector :trim ()
  "Free any extra space claimed by this vector."
  (setf @:vector (cl-subseq @:vector 0 @:fill)))

(def@ @vector :push (&rest elements)
  "Append ELEMENTS to the end of this vector."
  (prog1 @@
      (let ((count (length elements)))
        (while (< (- (length @:vector) @:fill) count)
          (@:grow)))
    (dolist (element elements)
      (setf (aref @:vector @:fill) element)
      (cl-incf @:fill))))

(def@ @vector :pop ()
  "Remove the element from the end of this vector and return it."
  (when (> @:fill 0)
    (prog1 (aref @:vector (cl-decf @:fill))
      (setf (aref @:vector @:fill) nil))))

(def@ @vector :shift ()
  "Remove element from the front of this vector (slow)."
  (unless (@:emptyp)
    (prog1 (@ @@ 0)
      (setf @:vector (cl-subseq @:vector 1))
      (cl-decf @:fill))))

(def@ @vector :unshift (&rest elements)
  "Add elements from to the front of this vector (slow), returning this."
  (prog1 @@
      (setf @:vector (cl-concatenate 'vector elements @:vector))
    (cl-incf @:fill (length elements))))

(def@ @vector :swap (i j)
  "Swap elements I and J in this vector, returning this vector."
  (prog1 @@
    (unless (= i j)
      (cl-psetf (@ @@ i) (@ @@ j)
                (@ @@ j) (@ @@ i)))))

(def@ @vector :to-list ()
  "Return the contents of this vector as a list."
  (cl-coerce (cl-subseq @:vector 0 @:fill) 'list))

(def@ @vector :clone ()
  "Make a shallow copy of this vector."
  (@extend @@ :vector (cl-copy-seq @:vector) :fill @:fill
              :infinite @:infinite :vector-default @:vector-default))

(def@ @vector :get (n)
  "Dynamic getter: get Nth element from this vector."
  (if (not (integerp n))
      (@^:get n)
    (if (< n @:fill)
        (aref @:vector n)
      (if @:infinite
          @:vector-default
        (signal 'args-out-of-range (list (cl-subseq @:vector 0 @:fill) n))))))

(def@ @vector :set (n value)
  "If N is an integer, sets the index in the vector to VALUE."
  (if (not (integerp n))
      (@^:set n value)
    (while (>= n (length @:vector))
      (@:grow))
    (setf (aref @:vector n) value
          @:fill (max (1+ n) @:fill))))

(provide '@vector)

;;; @vector.el ends here
