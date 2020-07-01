#lang racket/base

(require "../utils.rkt"
         "./boolean.rkt")

;; (check-lc "consequent" (if true "consequent" "alternative"))
;; (check-lc "alternative" (if false "consequent" "alternative"))

;; (check-lc true (and true true))
;; (check-lc false (and true false))
;; (check-lc false (and false true))
;; (check-lc false (and false false))

;; (check-lc true (or true true))
;; (check-lc true (or  true false))
;; (check-lc true (or  false true))
;; (check-lc false (or  false false))

;; (check-lc true (not false))
;; (check-lc false (not true))
