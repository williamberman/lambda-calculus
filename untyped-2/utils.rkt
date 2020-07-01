#lang racket/base

(provide check-lc)

(require rackunit
         "./reader.rkt"
         "./printer.rkt")

(define-syntax-rule (check-lc expected-result lc-expression)
  (check-equal? (lc:print (lc-> expected-result)) (lc:print (lc-> lc-expression))))
