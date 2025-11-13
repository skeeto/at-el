;;; @.el --- multiple-inheritance prototype-based objects DSL -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/at-el
;; Version: 1.5
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; @ is a library providing a domain-specific language for
;; multiple-inheritance prototype-based objects in Emacs Lisp. The
;; goal is to provide a platform for elegant object-oriented Emacs
;; Lisp.

;; @ performance benefits significantly from byte-compilation.

;; See README.md for a demonstration.

;;; Code:

(require 'gv)
(require 'cl-lib)

(with-no-warnings
  (defvar @ [@ (:proto ())]
    "The root object of the @ object system."))

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
  (cl-remove-duplicates
   (append (plist-get (aref object 1) :proto)
           (cl-mapcan #'@precedence (plist-get (aref object 1) :proto)))))

(defun @is (object proto)
  "Return t if OBJECT is an instance of PROTO."
  (and (@p object)
       (or (eq object proto)
           (and (memq proto (@precedence object)) t))))

(defsubst @--queue-create ()
  "Create a new empty queue object."
  (cons nil nil))

(defsubst @--queue-head (queue)
  "Return the head of QUEUE without modification."
  (car queue))

(defun @--queue-enqueue (queue value)
  "Add VALUE to the end of QUEUE."
  (let ((new (cons value nil)))
    (prog1 value
      (if (cdr queue)
          (setf (cdr (cdr queue)) new)
        (setf (car queue) new))
      (setf (cdr queue) new))))

(defun @--queue-dequeue (queue)
  "Remove and return the front of QUEUE."
  (if (eq (car queue) (cdr queue))
      (prog1 (caar queue)
        (setf (car queue) nil
              (cdr queue) nil))
    (pop (car queue))))

(cl-defun @ (object property &key (super 0) (default nil defaulted))
  "Find and return PROPERTY for OBJECT in the prototype chain.

If :super is t, skip the first match in the prototype chain.
If :default, don't produce an error but return the provided value."
  (let ((queue (@--queue-create)))
    (@--queue-enqueue queue object)
    (cl-loop while (@--queue-head queue)
             with skip = super
             for plist = (aref (@--queue-dequeue queue) 1)
             for pair = (plist-member plist property)
             when pair do (if (zerop skip)
                              (cl-return (cl-second pair))
                            (cl-decf skip))
             do (dolist (parent (plist-get plist :proto))
                  (@--queue-enqueue queue parent))
             finally return
             (if defaulted
                 default
               (@! object :get property)))))

(defvar @--super 0
  "Dynamic variablee to trace super method call or super property access.")
(cl-defun @--super (object property &key (default nil defaulted))
  (let ((@--super (+ 1 @--super)))
    (@ object property :super @--super :default default)))

(defun @--set (object property new-value)
  "Set the PROPERTY of OBJECT to NEW-VALUE."
  (@! object :set property new-value))

(gv-define-simple-setter @ @--set)

(defun @! (object property &rest args)
  "Call the method stored in PROPERTY with ARGS."
  (apply (@ object property) object args))

(defun @--super! (object property &rest args)
  "Call the method stored in PROPERTY with ARGS."
  (let ((@--super (+ 1 @--super)))
    (apply (@ object property :super @--super) object args)))

(cl-eval-when (compile load eval)
  (defun @--walk (sexp skip replace &optional head)
    "Replace all symbols by calling REPLACE on them."
    (cl-macrolet ((wrap (exp) `(let ((v ,exp)) (if head (list v) v))))
      (cond
       ((symbolp sexp) (funcall replace sexp head))
       ((atom sexp) (wrap sexp))
       ((member (cl-first sexp) skip) (wrap sexp))
       ((wrap
         (append (@--walk (cl-first sexp) skip replace t)
                 (cl-loop for element in (cdr sexp)
                          collect (@--walk element skip replace nil)))))))))

(cl-eval-when (compile load eval)
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
                   `(@--super! @@ ,property)
                 `(@--super @@ ,property))))
            (t (if head (list symbol) symbol))))))

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
           (cl-function
            (lambda ,(cons '@@ params)
              ,@(if (stringp (car body)) (list (car body)) ())
              (with-@@ @@
                (ignore @@)
                ,@(if (stringp (car body)) (cdr body) body)))))
     ,method))

(font-lock-add-keywords 'emacs-lisp-mode
  ;; "(\\<\\(def@\\)\\> +\\([^ ()]+\\)"
  '(("(\\<\\(def@\\) +\\([^ ()]+\\)"
     (1 'font-lock-keyword-face)
     (2 'font-lock-function-name-face))))

(font-lock-add-keywords 'emacs-lisp-mode
  ;; "\\<\\(@\\^?:[^ ()]+\\)\\>"
  '(("\\(@\\^?:[^ ()]+\\)\\>"
     (1 'font-lock-builtin-face))))

;; Core methods

(setf (aref @ 1) ; Bootstrap :set
      (plist-put (aref @ 1) :set
                 (lambda (@@ property new-value)
                   (setf (aref @@ 1)
                         (plist-put (aref @@ 1) property new-value))
                   new-value)))

(def@ @ :get (property)
  "Dynamic property getter. This one produces an error."
  (error "Property unbound: %s" property))

(def@ @ :init ())

(def@ @ :new (&rest args)
  "Extend this object and call the constructor method (:init) with ARGS."
  (let ((object (@extend @@)))
    (apply (@ object :init) object args)
    object))

(def@ @ :is (object)
  "Return t if this object is an instance of OBJECT."
  (@is @@ object))

(def@ @ :keys ()
  "Return a list of the keys directly on @@."
  (cl-loop for (key _) on (aref @@ 1) by #'cddr collect key))

(def@ @ :delete (to-delete)
  "Delete the key designated by TO-DELETE from the object."
  (let ((new nil))
    (cond ((eq to-delete :proto)
           (plist-put (aref @@ 1) :proto nil))
          (t
           (cl-loop for (key val) on (aref @@ 1) by #'cddr
                    do (unless (eq key to-delete)
                         (push val new)
                         (push key new)))
           (setf (aref @@ 1) new)))))
                    
;; Top-level Object Management

(defun @--list-all ()
  "List all global prototypes that start with @."
  (cl-flet ((protop (atom) (and (boundp atom)
                                (@p (symbol-value atom))
                                (= ?@ (aref (symbol-name atom) 0)))))
    (let ((list))
      (mapatoms (lambda (atom) (if (protop atom) (push atom list))))
      list)))

(defun describe-@ (proto property)
  "Like `describe-function' but for global protoype methods."
  (interactive
   (let* ((protos (mapcar #'symbol-name (@--list-all)))
          (prompt0 "Describe prototype: ")
          (symbol (intern (completing-read prompt0 protos nil t "@")))
          (proto (symbol-value symbol))
          (props (@! proto :keys))
          (methods (cl-remove-if-not
                    (lambda (p) (functionp (@ proto p))) props))
          (method-names (mapcar #'symbol-name methods))
          (prompt1 "Describe property: ")
          (property (intern (completing-read prompt1 method-names nil t ":"))))
     (list proto property)))
  (describe-function (@ proto property)))

(global-set-key (kbd "C-h @") 'describe-@)

(defun @--undefine-all ()
  "Undefine all public prototypes. Useful for reloading when debugging."
  (interactive)
  (mapc #'makunbound (@--list-all)))

(defun @--byte-compile-all ()
  "Byte-compile all public prototype methods."
  (interactive)
  (dolist (proto (mapcar #'symbol-value (@--list-all)))
    (dolist (prop (@! proto :keys))
      (when (functionp (@ proto prop))
        (byte-compile (@ proto prop))))))

(provide '@)

;;; @.el ends here
