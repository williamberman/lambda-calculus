#lang s-exp "../untyped/lang.rkt"

(provide Number
         String
         Boolean
         LCBoolean)

(require "core.rkt"
         "../untyped/stdlib/stdlib.rkt")

(define-base-type Number number?)
(define-base-type String string?)
(define-base-type Boolean boolean?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-lc-type LCBoolean)

(: true LCBoolean)
(: false LCBoolean)
(: if LCBoolean a a a)
(: and LCBoolean LCBoolean LCBoolean)
(: or LCBoolean LCBoolean LCBoolean)
(: not LCBoolean LCBoolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define-lc-type LCPair a b)

;; (: pair a b (LCPair a b))
;; (: first (LCPair a b) a)
;; (: second (LCPair a b) b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-lc-type LCNatural)

(: c0 LCNatural)
(: c1 LCNatural)
(: c2 LCNatural)
(: c3 LCNatural)
(: add LCNatural LCNatural LCNatural)
(: subtract LCNatural LCNatural LCNatural)
(: multiply LCNatural LCNatural LCNatural)
(: power LCNatural LCNatural LCNatural)
(: zero? LCNatural LCBoolean)
(: equal? LCNatural LCNatural LCBoolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define-lc-type LCList a)
;; (: cons a (LCList a) (LCList a))
;; (: nil (LCList Any))
;; (: nil? (LCList a) LCBoolean)
;; (: head (LCList a) a)
;; (: tail (LCList a) (LCList a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

