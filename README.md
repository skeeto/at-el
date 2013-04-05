# @, another object system for Emacs Lisp

@ is a library providing a domain-specific language for
multiple-inheritance prototype-based objects in Emacs Lisp. The goal
is to provide a platform for elegant object-oriented Emacs Lisp.

The root object of the @ object system is `@`. New objects are created
with the `@extend` function, by extending existing objects. Given no
objects to extend, new objects will implicitly extend `@`. Keyword
arguments provided to `@extend` are assigned as properties on the new
object.

Properties are looked up using `eq`, so stick to keywords, symbols,
and integers as property keys. The parent prototypes of an object,
used in property lookups, are listed in the `:proto` property of the
object. This can be modified at any time to change the prototype
chain.

## Feature Demonstration

Here's a hands-on example of @'s features. For a bunch of practical
examples check out the prototypes under lib/.

### Property Access

```el
(require '@)

;; Create a rectangle prototype, extending the root object @.
;; Convention: prefix "class" variable names with @.
(defvar @rectangle (@extend :width nil :height nil))

;; The @ function is used to access properties of an object, following
;; the prototype chain breadth-first as necessary. An error is thrown
;; if the property has not been defined.
(@ @rectangle :width) ; => nil

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
```

### Multiple Inheritance

```el
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
```

### Constructors and Super Methods

```el
;; By convention, constructors are the :init method. The @^:
;; "variables" are used to access super methods, including :init. Use
;; this to chain constructors and methods up the prototype chain (like
;; CLOS's `call-next-method').
(def@ @rectangle :init (width height)
  (@^:init)
  (setf @:width width @:height height))

;; The :new method on @ extends @@ with a new object and calls :init
;; on it with the provided arguments.
(@! (@! @rectangle :new 13.2 2.1) :area) ; => 27.72
```

### Dynamic Property Getters

```el
;; If a property is not found in the prototype chain, the :get method
;; is used to determine the value.
(let ((o (@extend)))
  (def@ o :get (property &optional default)
    (format "got %s" property))
  (@ o :foo))
; => "got :foo"

;; The :get method on @, the default getter, produces an error if a
;; property is unbound. If you would rather unbound properties return
;; nil mix in @soft-get, which provides an alternate default :get
;; method.
(@ (@extend @rectangle @soft-get) :foo) ; => nil
```

### Reflection

```el
;; @is is the classical "instanceof" operator. It works on any type of
;; object in both positions.
(@is foo @colored)   ; => t
(@is foo @rectangle) ; => t
(@is foo foo)        ; => t
(@is foo @)          ; => t
(@is foo (@extend))  ; => nil
(@is [1 2 3] @)      ; => nil

;; The :is method on @ can also be used for this.
(@! @colored :is @)    ; => t
(@! foo :is @colored)  ; => t

;; The :keys method on @ can be used to list the keys on an object.
(@! foo :keys)  ; => (:proto :width :height :color)
```

### Syntax Highlighting

The library provides syntax highlighting for 'def@' and '@:' variables
in emacs-lisp-mode, so the above @ uses will look more official in an
Emacs buffer.
