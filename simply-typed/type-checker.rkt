#lang racket/base

(provide type-check
         type-exn?)

(require [for-syntax syntax/parse racket/base]
         "../untyped/core.rkt"
         "core.rkt"
         "utils.rkt")

(struct type-exn exn:fail ())

(define-syntax (type-check stx)
  (syntax-parse stx
    [(_ (func:expr argument:expr))
     (syntax
      (type-check-helper (type-check func) (type-check argument)))]
    ;; TODO what syntax class should I apply to literal
    [(_ literal:expr)
     (syntax
      (type-check-base literal))]))

(define (type-check-base term)  
  (cond [(lambda-abstraction? term)
         (get-type-tag term)]
        [(lc-type? term)
         term]
        [else (or (find-base-type term)
                  Any)]))

(define (type-check-helper func-type argument-type)  
  (set! argument-type (coerce-type argument-type))
  (set! func-type (substitute-type-variable func-type argument-type))
  
  (cond [(or (eq? Any func-type) (eq? Any argument-type))
         Any]
        [(not (function-signature? func-type))
         (raise (function-application-type-error func-type))]
        [(equal? (car func-type) argument-type)
         (cdr func-type)]
        [else
         (raise (type-mismatch-type-error (car func-type) argument-type))]))

(define (function-application-type-error func-type)  
  (type-error (format "Cannot apply expression of type ~a"
                      (print-type-signature func-type))))

(define (coerce-type type)
  (if (and (list? type)
           (null? (cdr type)))
      (car type)
      type))

(define (substitute-type-variable func-type argument-type)
  (if (and (function-signature? func-type)
           (type-variable? (car func-type)))
      (let ([substituting-symbol (type-variable-symbol (car func-type))])
        (map (lambda (type)
               (if (and (type-variable? type)
                        (eq? (type-variable-symbol type)
                             substituting-symbol))
                   argument-type
                   type))
             func-type))
      func-type))

(define (type-mismatch-type-error expected given)
  (type-error (format "Type mismatch\n  expected: ~a\n  given: ~a"
                      (print-type-signature expected)
                      (print-type-signature given))))

(define (type-error message)
  (type-exn (format "Type Checker: ~a" message)
            (current-continuation-marks)))
