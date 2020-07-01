#lang racket/base

(provide top-interaction
         module-begin
         lookup-binding
         repl-eval!)

(require "interpreter.rkt"
         "printer.rkt"
         (for-syntax racket/base syntax/parse))

(define *env* (make-immutable-hash))

(define (lookup-binding binding)
  (hash-ref *env*
            binding
            (lambda ()
              (error 'lookup-binding "Not bound ~a" binding))))

(define (repl-eval! form)
  (define result (lc:eval form *env*))
  (set! *env* (cdr result))
  (car result))

(define-syntax (top-interaction stx)
  (syntax-parse stx
    [(_ . form:expr)
     (syntax
      (lc:print (repl-eval! form)))]))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ module-body:expr ...)
     (syntax
      (#%module-begin       
       (module-begin-helper module-body ...)))]))

(define-syntax (module-begin-helper stx)
  (syntax-parse stx
    #:literals (require)
    [(_ (require form:expr) rest-module-body:expr ...)
     (syntax
      (begin
        (require form)
        (module-begin-helper rest-module-body ...)))]
    [(_)
     (syntax (void))]
    [(_ module-body:expr rest-module-body:expr ...)
     (syntax
      (begin
        (lc:print (repl-eval! module-body))
        (module-begin-helper rest-module-body ...)))]))
