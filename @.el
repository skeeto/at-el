;;; @.el --- multiple-inheritance prototype-based objects DSL

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <mosquitopsu@gmail.com>
;; URL: https://github.com/skeeto/skewer-mode
;; Version: 1.4
;; Package-Requires: ((queue "0.1"))

;;; Commentary:

;; @ is a library providing a domain-specific language for
;; multiple-inheritance prototype-based objects in Emacs Lisp. The
;; goal is to provide a platform for elegant object-oriented Emacs
;; Lisp.

;; See README.md for a demonstration.

;;; Code:

(require 'cl)
(require 'queue)

(defvar @ [@ (:proto ())]
  "The root object of the @ object system.")

(defun @p (object)
  "Return t if OBJECT is an @ object."
  (and (vectorp object) (eq '@ (aref object 0))))

(defun @extend (&rest args)
  "Create a new object extending zero or more prototypes, binding
the given property/value pairs as properties. If no prototypes
are provided, extend @."
  (let* ((objects ()))
    (while (@p (car args))
      (push (pop args) objects))
    (when (null objects) (push @ objects))
    (vector '@ `(:proto ,(nreverse objects) ,@args))))

(defun @precedence (object)
  "Return the lookup precedence order for OBJECT."
  (remove-duplicates
   (append (plist-get (aref object 1) :proto)
           (mapcan #'@precedence (plist-get (aref object 1) :proto)))))

(defun @is (object proto)
  "Return t if OBJECT is an instance of PROTO."
  (and (@p object)
       (or (eq object proto)
           (and (memq proto (@precedence object)) t))))

(defun* @ (object property &key super)
  "Find and return PROPERTY for OBJECT in the preototype chain."
  (let ((queue (make-queue)))
    (queue-enqueue queue object)
    (loop while (queue-head queue)
          with skip = (if super 1 0)
          for plist = (aref (queue-dequeue queue) 1)
          for pair = (plist-member plist property)
          when pair do (if (zerop skip)
                           (return (second pair))
                         (decf skip))
          do (dolist (parent (plist-get plist :proto))
               (queue-enqueue queue parent))
          finally (error "Property unbound: %s" property))))

(defun @--set (object property new-value)
  "Set the PROPERTY of OBJECT to NEW-VALUE."
  (setf (aref object 1) (plist-put (aref object 1) property new-value))
  new-value)

(defsetf @ @--set)

(defun @! (object property &rest args)
  "Call the method stored in PROPERTY with ARGS."
  (apply (@ object property) object args))

(defun @--walk (sexp skip replace &optional head)
  "Replace all symbols by calling REPLACE on them."
  (macrolet ((wrap (exp) `(let ((v ,exp)) (if ,head (list v) v))))
    (cond
     ((symbolp sexp) (funcall replace sexp head))
     ((atom sexp) (wrap sexp))
     ((member (first sexp) skip) (wrap sexp))
     ((wrap
       (nconc (@--walk (first sexp) skip replace t)
              (loop for element in (cdr sexp)
                    collect (@--walk element skip replace nil))))))))

(defun @--replace (symbol head)
  "Replace @: and @^: symbols with their lookup/funcall expansions."
  (let ((name (symbol-name symbol)))
    (cond ((string-prefix-p "@:" name)
           (let ((property (intern (substring name 1))))
             (if head
                 `(@! @@ ,property)
               `(@ @@ ,property))))
          ((string-prefix-p "@^:" name)
           (let ((property (intern (substring name 2))))
             (if head
                 `(funcall (@ @@ ,property :super t))
               `(@ @@ ,property :super t))))
          (t (if head (list symbol) symbol)))))

(defmacro with-@@ (object &rest body)
  "Provide the @: and @^: DSL utilities for OBJECT in BODY."
  (declare (indent defun))
  `(let ((@@ ,object))
     ,@(cdr (@--walk (cons 'progn body) '(quote with-@@) #'@--replace))))

(defmacro def@ (object method params &rest body)
  "Define METHOD body on OBJECT."
  (declare (indent defun))
  `(progn
     (setf (@ ,object ,method)
           (function* (lambda ,(cons '@@ params)
                        ,@(if (stringp (car body)) (list (car body)) ())
                        (with-@@ @@
                          ,@(if (stringp (car body)) (cdr body) body)))))
     ,method))

(font-lock-add-keywords 'emacs-lisp-mode
  '(("(\\<\\(def@\\)\\> +\\([^ ()]+\\)"
     (1 'font-lock-keyword-face)
     (2 'font-lock-function-name-face))))

(font-lock-add-keywords 'emacs-lisp-mode
  '(("\\<\\(@\\^?:[^ ()]+\\)\\>"
     (1 'font-lock-builtin-face))))

;; Core methods

(def@ @ :init ())

(def@ @ :new (&rest args)
  "Extend this object and call the constructor (init) method with ARGS."
  (let ((object (@extend @@)))
    (apply (@ object :init) object args)
    object))

(def@ @ :is (object)
  "Return t if this object is an instance of OBJECT."
  (@is @@ object))

;; Local Variables:
;; lexical-binding: t
;; End:

(provide '@)

;;; @.el ends here
