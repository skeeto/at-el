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

;; @ performance benefits significantly from byte-compilation.

;; See README.md for a demonstration.

;;; Code:

(require 'cl)
(require 'queue)

;; Boxes

(defconst @--box-empty-cell (gensym))

(defun @--box (value parent-value)
  "Box VALUE."
  (cons value parent-value))

(defun @--box-set (box value)
  "Set the value in the box."
  (setf (car box) value))

(defun @--box-get (box)
  "Retrieve the value in the box."
  (if (eq (car box) @--box-empty-cell)
      (@--box-get (cdr box))
    (car box)))

;; @

(defvar @ (vector '@ (list :proto (@--box () @--box-empty-cell)))
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
    (vector '@ `(:proto ,(@--box (nreverse objects) @--box-empty-cell)
                        ,@(loop for (pname pval) on args by #'cddr
                                nconc (list pname (@--box pval @--box-empty-cell)))))))

(defun @precedence (object)
  "Return the lookup precedence order for OBJECT."
  (remove-duplicates
   (let ((proto (@--box-get (plist-get (aref object 1) :proto))))
    (append proto (mapcan #'@precedence proto)))))

(defun @is (object proto)
  "Return t if OBJECT is an instance of PROTO."
  (and (@p object)
       (or (eq object proto)
           (and (memq proto (@precedence object)) t))))

(defun* @ (object property &key super (default nil defaulted))
  "Find and return PROPERTY for OBJECT in the prototype chain.

If :super is t, skip the first match in the prototype chain.
If :default, don't produce an error but return the provided value."
  (let ((mem (plist-member (aref object 1) property)))
    (if (and (not super) mem)
        ;; fast path
        (@--box-get (second mem))
      ;; walk the prototype chain
      (let ((queue (make-queue)))
        (queue-enqueue queue object)
        (loop while (queue-head queue)
              with skip = (if super 1 0)
              for obj = (queue-dequeue queue)
              for plist = (aref obj 1)
              for pair = (plist-member plist property)
              when pair do (if (zerop skip)
                               (progn
                                 (unless (or super (eq obj object))
                                   ;; cache the answer
                                   (setf (aref object 1)
                                         (plist-put (aref object 1) property
                                                    (@--box @--box-empty-cell (second pair)))))
                                 (return (@--box-get (second pair))))
                             (decf skip))
              do (dolist (parent (@--box-get (plist-get plist :proto)))
                   (queue-enqueue queue parent))
              finally (return
                       (if defaulted
                           default
                         (@! object :get property))))))))

(defun @--set (object property new-value)
  "Set the PROPERTY of OBJECT to NEW-VALUE."
  (@! object :set property new-value))

(defsetf @ @--set)

(defun @! (object property &rest args)
  "Call the method stored in PROPERTY with ARGS."
  (apply (@ object property) object args))

(defun @--walk (sexp skip replace &optional head)
  "Replace all symbols by calling REPLACE on them."
  (macrolet ((wrap (exp) `(let ((v ,exp)) (if head (list v) v))))
    (cond
     ((symbolp sexp) (funcall replace sexp head))
     ((atom sexp) (wrap sexp))
     ((member (first sexp) skip) (wrap sexp))
     ((wrap
       (append (@--walk (first sexp) skip replace t)
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
                 `(funcall (@ @@@ ,property :super t) @@)
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
                        (let ((@@@ ,object))
                          (with-@@ @@
                            ,@(if (stringp (car body)) (cdr body) body))))))
     ,method))

(font-lock-add-keywords 'emacs-lisp-mode
  '(("(\\<\\(def@\\)\\> +\\([^ ()]+\\)"
     (1 'font-lock-keyword-face)
     (2 'font-lock-function-name-face))))

(font-lock-add-keywords 'emacs-lisp-mode
  '(("\\<\\(@\\^?:[^ ()]+\\)\\>"
     (1 'font-lock-builtin-face))))

;; Core methods

(setf (aref @ 1) ; Bootstrap :set
      (plist-put (aref @ 1) :set
                 (@--box
                  (lambda (@@ property new-value)
                    (let* ((props (aref @@ 1))
                           (box (plist-get props property)))
                      (if box
                          (@--box-set box new-value)
                        (setf (aref @@ 1)
                              (plist-put props property (@--box new-value @--box-empty-cell)))))

                    new-value)
                  @--box-empty-cell)))

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
  (loop for (key value) on (aref @@ 1) by #'cddr collect key))

;; Other useful prototypes

(defvar @soft-get (@extend :default-get nil)
  "Mixin: don't throw errors on unbound properties.")

(def@ @soft-get :get (_property)
  "If no DEFAULT is provided for PROPERTY, return @:default-get."
  @:default-get)

(defvar @immutable (@extend :immutable-error t)
  "Don't allow changes on this object. Causes error if :immutable-error is t.")

(def@ @immutable :set (property _value)
  "Don't allow setting of properties on this object."
  (when @:immutable-error
    (error "Object is immutable, cannot set %s" property)))

(defvar @watchable (@extend :watchers nil)
  "Allow subscribing to changes to this object.")

(def@ @watchable :watch (callback)
  "Subscribe to this object's changes."
  (push callback @:watchers))

(def@ @watchable :unwatch (callback)
  "Subscribe to this object's changes."
  (setf @:watchers (remove callback @:watchers)))

(def@ @watchable :set (property new)
  (dolist (callback @:watchers)
    (funcall callback @@ property new))
  (@^:set property new))

;; Top-level Object Management

(defun @--list-all ()
  "List all global prototypes that start with @."
  (flet ((protop (atom) (and (boundp atom)
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
          (methods (remove-if-not (lambda (p) (functionp (@ proto p))) props))
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

;; Local Variables:
;; lexical-binding: t
;; End:

(provide '@)

;;; @.el ends here
