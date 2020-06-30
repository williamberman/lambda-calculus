#lang racket/base

(provide lc:print)

(require "core.rkt")

(define (lc:print term-or-terms)
  (cond
    [(list? term-or-terms) (if (null? term-or-terms)
                               (list)
                               (cons (lc:print (car term-or-terms))
                                     (lc:print (cdr term-or-terms))))]
    [(lc:abstraction? term-or-terms) (lc:print-abstraction term-or-terms)]
    [(lc:assignment? term-or-terms) (lc:print-assignment term-or-terms)]
    [(lc:application? term-or-terms) (lc:print-application term-or-terms)]
    [(lc:variable? term-or-terms) (lc:print-variable term-or-terms)]
    [(lc:native-data-type? term-or-terms) (lc:print-native-data-type term-or-terms)]
    [else (error 'lc:print "Unknown term ~a" term-or-terms)]))

(define (lc:print-abstraction abstraction)
  `(lambda ,(lc:abstraction-binding abstraction)
     ,@(lc:print (lc:abstraction-body abstraction))))

(define (lc:print-assignment assignment)
  `(define ,(lc:assignment-binding assignment)
     ,@(lc:print (lc:assignment-body assignment))))

(define (lc:print-application application)
  `(,(lc:print (lc:application-recipient application))
    ,(lc:print (lc:application-argument application))))

(define (lc:print-variable variable)
  (lc:variable-binding variable))

(define (lc:print-native-data-type native)
  (lc:native-data-type-data native))
