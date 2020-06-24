#lang racket/base

(provide define-base-type
         define-type
         :
         type-of
         type-check)

(require [for-syntax syntax/parse racket/base])

(define-syntax (define-base-type stx)
  (syntax-parse stx
    [(_ identifier:id predicate:expr)
     (syntax
      (define identifier (make-base-type 'identifier predicate)))]))

(define-syntax (define-type stx)
  (syntax-parse stx
    ;; TODO parameters:<what>
    [(_ identifier:id parameters ...)
     (syntax
      (define identifier (make-derived-type 'identifier
                                            (list parameters ...)
                                            (lambda args
                                              "TODO"))))]))

(define-struct base-type (identifier predicate))

(define-struct derived-type (identifier parameters type-constructor)
  #:property prop:procedure (struct-field-index type-constructor))

(define (: term . typings)
  "TODO")

(define (type-of term)
  "TODO")

(define (type-check term)
  "TODO")
