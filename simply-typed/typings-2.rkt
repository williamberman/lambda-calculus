#lang racket/base

(provide if-macro
         type-check
         x-eval
         true
         false
         set-type-checker!)

(require [for-syntax syntax/parse racket/base]
         data/gvector)

(struct type-exn exn:fail ())

(define *type-checker* #t)

(define (set-type-checker! mode)
  (set! *type-checker* mode))

(define Boolean 'Boolean)

(define Number 'Number)

(define Any 'Any)

(define true #t)

(define false #f)

(define (x-eval term)
  (when *type-checker*
    (type-check term))
  
  (cond
    [(if-struct? term) (if-struct-eval term)]
    [(boolean? term) term]
    [(number? term) term]))

(define (if-struct-eval if-struct)
  (if (x-eval (if-struct-predicate if-struct))
      (x-eval (if-struct-consequent if-struct))
      (x-eval (if-struct-alternative if-struct))))

(define-syntax (if-macro stx)
  (syntax-parse stx
    [(_ predicate consequent alternative)
     (syntax
      (make-if-struct predicate consequent alternative))]))

(define-struct if-struct (predicate consequent alternative))

(define (if-struct-type-check if-struct)
  (check! Boolean (type-check (if-struct-predicate if-struct)))
  (define consequent-type (type-check (if-struct-consequent if-struct)))
  (define alternative-type (type-check (if-struct-alternative if-struct)))
  (check! consequent-type
          alternative-type
          "Arms of conditional of have different types")
  (unify consequent-type alternative-type))

(define *type-checkers* (make-gvector))

(define-struct type-checker (predicate checker))

(gvector-add! *type-checkers* (make-type-checker boolean? (lambda args Boolean)))
(gvector-add! *type-checkers* (make-type-checker number? (lambda args Number)))
(gvector-add! *type-checkers* (make-type-checker if-struct? if-struct-type-check))

(define (type-check term)
  (define type-checker (findf (lambda (type-checker)
                                ((type-checker-predicate type-checker) term))
                              (gvector->list *type-checkers*)))

  (if type-checker
      ((type-checker-checker type-checker) term)
      Any))

(define (check! expected given [message #f])
  (when (not (type-equal? expected given))
    (raise (type-error expected given message))))

(define (type-error expected given [message #f])
  (when (not message)
    (set! message
          (format "Type Checker: type mismatch\nexpected: ~a\ngiven: ~a"
                  expected
                  given)))
  (type-exn message
            (current-continuation-marks)))

(define (type-equal? type1 type2)
  (if (or (eq? Any type1)
          (eq? Any type2))
      #t
      (equal? type1 type2)))

(define (unify type1 type2)
  (if (or (eq? Any type1)
          (eq? Any type2))
      Any
      (if (equal? type1 type2)
          type1
          (error 'unify "Could not unify ~a and ~a" type1 type2))))

;; > (if (if true false true) 0 1)
; - : Number
; 0

;; > (if 0 true false)
; Type Checker: type mismatch
;   expected: Boolean
;   given: Number

;; > (if true 0 false)
; Type Checker: type mismatch
;   arms of conditional have different types
