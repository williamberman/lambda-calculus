#lang s-exp "../untyped/lang.rkt"

(require "core.rkt"
         "../untyped/stdlib/stdlib.rkt")

(define-base-type Number number?)
(define-base-type String string?)
(define-base-type Boolean boolean?)

(define-lc-type LCBoolean)

(: true LCBoolean)
(: false LCBoolean)
(: if LCBoolean a a a)
