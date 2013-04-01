# @, another object system for Emacs Lisp

@ is a library providing a domain-specific language for
multiple-inheritance prototype-based objects in Emacs Lisp. The goal
is to provide a platform for elegant object-oriented Emacs Lisp.

The root object of the @ object system is `@`. The `@extend` function
is used to create new objects extending other objects. Given no
objects to extend, new objects will implicitly extend `@`. Keyword
arguments provided to `@extend` are assigned as properties on the new
object.

Properties are looked up using `eq`, so stick to keywords, symbols,
and integers as property keys. The parent prototypes of an object,
used in property lookups, are listed in the `:proto` property of the
object. This can be modified at any time to change the prototype
chain.

```el
(require '@)

;; Create a rectangle prototype, extending the root object @.
;; Convention: prefix "class" variable names with @.
(defvar @rectangle (@extend :width nil :height nil))

;; The @ function is used to access properties of an object, following
;; the prototype chain breadth-first as necessary.
(@ @rectangle :width)  ; => nil

;; The @ function is setf-able. Assignment *always* happens on the
;; immediate object, never on a parent prototype.
(setf (@ @rectangle :width) 0)
(setf (@ @rectangle :height) 0)

;; Define the method :area on @rectangle.
;; The first argument is this/self. Convention: call it @@.
(setf (@ @rectangle :area) (lambda (@@) (* (@ @@ :width) (@ @@ :height))))

;; Convenience macro def@ for writing methods. Symbols like @: will be
;; replaced by lookups on @@. The following is equivalent to the above
;; definition.
(def@ @rectangle :area ()
  (* @:width @:height))

;; Create a color mix-in prototype
(defvar @colored (@extend :color (list)))

;; The @: variables are setf-able, too.
(def@ @colored :mix (color)
  (push color @:color))

;; Create a colored rectangle from the prototypes.
(defvar foo (@extend @colored @rectangle :width 10 :height 4))

;; @! is used to call methods. The object itself is passed as the
;; first argument to the function stored on that prototype's property.
(@! foo :area)  ; => 40
(@! foo :mix :red)
(@! foo :mix :blue)
(@ foo :color)  ; => (:blue :red)

;; @: variables are turned into method calls when in function position.
(def@ foo :describe ()
  (format "{color: %s, area: %d}" @:color (@:area)))

(@! foo :describe)  ; => "{color: (:blue :red), area: 40}"

;; @of is the classical "instanceof" operator.
(@of foo @colored)   ; => t
(@of foo @rectangle) ; => t
(@of foo foo)        ; => t
(@of foo @)          ; => t
(@of foo (@extend))  ; => nil
```

The library provides syntax highlighting for '@def' and '@:' variables
in emacs-lisp-mode, so the above @ uses will look more official in an
Emacs buffer.
