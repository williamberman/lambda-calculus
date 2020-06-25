#lang racket/base

(provide type-checker-on!
         type-checker-off!
         type-checker?
         (rename-out [top-interaction-wrapper top-interaction]))

(require "type-checker.rkt"
         "utils.rkt"
         "../untyped/top-interaction.rkt"
         [for-syntax syntax/parse racket/base])

(define *type-checking* #t)

(define (type-checker-on!)
  (set! *type-checking* #t))

(define (type-checker-off!)
  (set! *type-checking* #f))

(define (type-checker?)
  (if *type-checking*
      'on
      'off))

;; TODO top level set! does not work
(define-syntax (top-interaction-wrapper stx)
  (syntax-parse stx
    #:literals (define require)
    [(_ define form:expr ...) (syntax                               
                               (define form ...))]
    
    [(_ require form:expr ...) (syntax                                
                                (require form ...))]
    
    [(_ form:expr arg:expr) (syntax                             
                             (if *type-checking*
                                 (let ([type-result (print-type-signature
                                                     (type-check (form arg)))])

                                   (call-with-values (lambda ()
                                                       (top-interaction form arg))
                                                     (lambda result
                                                       (apply values
                                                              (cons type-result result)))))
                                 (top-interaction form arg)))]

    [(_ . form:expr) (syntax
                      (if *type-checking*
                          (let ([type-result (print-type-signature
                                              (type-check form))])
                            
                            (call-with-values (lambda ()
                                                (top-interaction . form))
                                              (lambda result
                                                (apply values
                                                       (cons type-result result)))))
                          (top-interaction . form)))]))
