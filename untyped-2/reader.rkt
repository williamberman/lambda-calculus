#lang racket/base

(provide lc:assignment-macro
         lc:variable-macro
         lc:application-macro
         lc:abstraction-macro
         lc:native-data-type-macro)

(require "core.rkt"
         [for-syntax syntax/parse racket/base])

(define-syntax (lc:assignment-macro stx)
  (syntax-parse stx
    [(_ binding:id body:expr ...+)
     (syntax
      (lc:assignment 'binding (list body ...)))]))

(define-syntax (lc:variable-macro stx)
  (syntax-parse stx
    [(_ . binding:id)
     (syntax
      (lc:variable 'binding))]))

(define-syntax (lc:application-macro stx)
  (syntax-parse stx
    [(_ recipient:expr argument:expr)
     (syntax
      (lc:application recipient argument))]))

(define-syntax (lc:abstraction-macro stx)
  (syntax-parse stx
    [(_ binding:id body:expr ...+)
     (syntax
      (lc:abstraction 'binding (list body ...)))]))

(define-syntax (lc:native-data-type-macro stx)
  (syntax-parse stx
    [(_ . data:expr)
     (syntax
      (lc:native-data-type (#%datum . data)))]))
