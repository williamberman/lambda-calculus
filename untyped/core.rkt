#lang racket/base

(require [for-syntax syntax/parse racket/base]
         "../utils.rkt")

(provide (rename-out [lambda-abstraction-stx make-lambda-abstraction])
         lambda-abstraction-binding
         lambda-abstraction-body
         lambda-abstraction?
         map-lambda-abstraction-bindings)

(define-syntax (lambda-abstraction-stx stx)
  (syntax-parse stx
    [(_ variable:id body:expr)
     (syntax      
      (make-lambda-abstraction 'variable 'body (lambda (variable) body)))]))

(define-struct lambda-abstraction (binding body func)
  #:property prop:procedure (struct-field-index func))

(define (map-lambda-abstraction-bindings lambda-abstraction mapper)  
  (tree-map mapper (lambda-abstraction-body mapper)))