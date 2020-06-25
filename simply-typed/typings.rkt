#lang s-exp "../untyped/lang.rkt"

(provide Number
         String
         Boolean
         LCBoolean
         )

(require "core.rkt"
         "../untyped/stdlib/stdlib.rkt")

(define-base-type Number number?)
(define-base-type String string?)
(define-base-type Boolean boolean?)

(define-lc-type LCBoolean)

(: true LCBoolean)
(: false LCBoolean)
(: if LCBoolean a a a)
