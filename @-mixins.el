;;; @-mixins.el --- useful mixin prototypes for @

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; The mixins provided:

;; * @soft-get
;; * @immutable
;; * @watchable

;; See each variable's documentation for more info.

;;; Code:

(require '@)

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

(provide '@-mixins)

;;; @-mixins.el ends here
