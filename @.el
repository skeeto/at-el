;;; @.el --- multiple-inheritance prototype-based objects DSL

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; @ is a library providing a domain-specific language for
;; multiple-inheritance prototype-based objects in Emacs Lisp. The
;; goal is to provide a platform for elegant object-oriented Emacs
;; Lisp.

;; See README.md for a demonstration.

;;; Code:

(require 'cl)

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

(defun @of (object proto)
  "Return t if OBJECT is an instance of PROTO."
  (and (@p object)
       (or (eq object proto)
           (and (memq proto (@precedence object)) t))))

(defun* @ (object property &optional (new-value nil set-mode))
  "Find and return PROPERTY for OBJECT in the prototype chain."
  (symbol-macrolet ((plist (aref object 1)))
    (if set-mode
        (setf plist (plist-put plist property new-value))
      (let ((pair (plist-member plist property)))
        (if pair
            (second pair)
          (loop for proto in (@precedence object)
                for pair = (plist-member (aref proto 1) property)
                when pair return (second pair)
                finally (error "Property unbound: %s" property)))))))

(defsetf @ @)

(defun @! (object property &rest args)
  "Call the method stored in PROPERTY with ARGS."
  (apply (@ object property) object args))

(defun @--deref (symbol)
  "Convert a @: symbol into a keyword symbol."
  (when (symbolp symbol)
    (let ((name (symbol-name symbol)))
      (when (and (>= (length name) 2) (string= "@:" (substring name 0 2)))
        (intern (substring name 1))))))

(defun @--walk (sexp &optional head)
  "Convert all @: symbols into lookups and funcalls."
  (if (consp sexp)
      (if (eq 'quote (car sexp))
          sexp
        (if (and head (@--deref (car sexp)))
            `(@! @@ ,(@--deref (car sexp)) ,@(@--walk (cdr sexp)))
          (cons (@--walk (car sexp) t) (@--walk (cdr sexp)))))
    (if (@--deref sexp)
        `(@ @@ ,(@--deref sexp))
      sexp)))

(defmacro def@ (object method params &rest body)
  "Define METHOD body on OBJECT."
  (declare (indent defun))
  `(progn
     (@ ,object ,method (lambda ,(cons '@@ params)
                          ,@(@--walk (macroexpand-all body))))
     ,method))

(font-lock-add-keywords 'emacs-lisp-mode
  '(("(\\<\\(def@\\)\\> +\\([^ ()]+\\)"
     (1 'font-lock-keyword-face)
     (2 'font-lock-function-name-face))))

(font-lock-add-keywords 'emacs-lisp-mode
  '(("\\<\\(@:[^ ()]+\\)\\>"
     (1 'font-lock-builtin-face))))

(provide '@)

;;; @.el ends here
