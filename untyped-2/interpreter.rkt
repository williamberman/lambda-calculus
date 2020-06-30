#lang racket/base

(provide lc:eval)

(require "core.rkt")

(define (lc:eval term-or-terms env)
  (cond
    [(list? term-or-terms) (lc:eval* term-or-terms env)]
    [(lc:abstraction? term-or-terms) (lc:eval-abstraction term-or-terms env)]
    [(lc:assignment? term-or-terms) (lc:eval-assignment term-or-terms env)]
    [(lc:application? term-or-terms) (lc:eval-application term-or-terms env)]
    [(lc:variable? term-or-terms) (lc:eval-variable term-or-terms env)]
    [(lc:native-data-type? term-or-terms) (lc:eval-native-data-type term-or-terms env)]
    [else (error 'lc:eval "Unknown term ~a" term-or-terms)]))

(define (lc:eval* terms env)
  (cond
    [(null? terms)
     (error 'lc:eval* "Cannot eval* no terms")]
    [(null? (cdr terms))
     (lc:eval (car terms)
              env)]
    [else
     (let ([next-env (cdr (lc:eval (car terms) env))])
       (lc:eval (cdr terms) next-env))]))

(define (lc:eval-abstraction abstraction env)
  (cons abstraction env))

(define (lc:eval-assignment assignment env)
  (define the-value (car (lc:eval (lc:assignment-body assignment) env)))
  
  (cons the-value
        (hash-set env
                  (lc:assignment-binding assignment)
                  the-value)))

(define (lc:eval-application application env)
  (define the-abstraction (car (lc:eval (lc:application-recipient application)
                                        env)))

  (define the-argument (car (lc:eval (lc:application-argument application)
                                     env)))

  (cons (lc:eval (lc:abstraction-body the-abstraction)
                 (hash-set env
                           (lc:abstraction-binding the-abstraction)
                           the-argument))
        env))

(define (lc:eval-variable variable env)
  (cons (hash-ref env
                  (lc:variable-binding variable)
                  (lambda () (error 'variable-lookup
                                    "~a is unbound"
                                    (lc:variable-binding variable))))
        env))

(define (lc:eval-native-data-type native env)
  (cons native env))
