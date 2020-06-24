#lang s-exp "../untyped/lang.rkt"

(require "../untyped/stdlib/stdlib.rkt")
(require "core.rkt")

(define-base-type Number number?)
(define-base-type String string?)
(define-base-type Boolean boolean?)

(define foo (lambda x "response"))
(: foo Number String)

(define bar (foo 1))
(type-of bar) ;; String

(define-type Pair 'a 'b)

(: pair 'a 'b (Pair 'a 'b))

(: first (Pair 'a 'b) 'a)

(: second (Pair 'a 'b) 'b)

(: if Boolean 'a 'a)

;; > pair
;; '(lambda fst
;;    (lambda snd
;;      (lambda choose
;;        ((choose fst) snd))))
;; - : (-> a b (Pair a b))

;; > (type-check (((if (second ((pair true) 1))) "first") "second"))
;; ; Type Checker: type mismatch
;; ;   expected: Boolean
;; ;   given: Number
