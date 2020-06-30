#lang racket/base

(provide top-interaction
         module-begin)

(require "interpreter.rkt"
         "printer.rkt"
         (for-syntax racket/base syntax/parse))

(define *env* (make-immutable-hash))

(define (repl-eval! form)
  (define result (lc:eval form *env*))
  (set! *env* (cdr result))
  (car result))

(define-syntax (top-interaction stx)
  (syntax-parse stx
    [(_ . form:expr)
     (syntax
      (lc:print (repl-eval! form)))]))

;; TODO look for example
(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ module-body:expr ...)
     (syntax
      (#%module-begin
       (repl-eval! (list module-body ...))))]))
