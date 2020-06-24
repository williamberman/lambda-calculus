#lang racket/base

(provide type-check
         type-exn?)

(require [for-syntax syntax/parse racket/base]
         "../untyped/core.rkt"
         "core.rkt")

(struct type-exn exn:fail ())

(define-syntax (type-check stx)
  (syntax-parse stx
    [(_ (func:expr argument:expr))
     (syntax
      (type-check-helper (type-check func) (type-check argument)))]
    ;; TODO what tag should I apply to literal
    [(_ literal)
     (syntax
      (type-check-base literal))]))

(define (type-check-base term)
  (if (lambda-abstraction? term)
      (get-type-tag term)      
      (let ([type-checker (find-type-checker term)])        
        (if type-checker
            (type-checker-identifier type-checker)
            Any))))

(define (type-check-helper func-type argument-type)
  (set! argument-type (coerce-type argument-type))
  
  (when (type-variable? (car func-type))
    (set! func-type (substitute-type-variable func-type argument-type)))  
  
  (if (equal? (car func-type) argument-type)
      (cdr func-type)
      (raise (type-error (car func-type) argument-type))))

(define (coerce-type type)
  (if (and (list? type)
           (null? (cdr type)))
      (car type)
      type))

(define (substitute-type-variable func-type argument-type)
  (define substituting-symbol (type-variable-symbol (car func-type)))
  (map (lambda (type)
         (if (and (type-variable? type)
                  (eq? (type-variable-symbol type)
                       substituting-symbol))
             argument-type
             type))
       func-type))

(define (type-error expected given)
  (type-exn (format "Type Checker: type mismatch\nexpected: ~a\ngiven: ~a"
                    expected
                    given)
            (current-continuation-marks)))
