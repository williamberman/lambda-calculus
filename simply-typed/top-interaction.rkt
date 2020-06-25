#lang racket/base

(provide (rename-out [top-interaction-wrapper top-interaction]))

(require "type-checker.rkt"
         "utils.rkt"
         "../untyped/top-interaction.rkt"
         [for-syntax syntax/parse racket/base])

;; TODO allow type checking to be turned on or off
(define *type-checking* #t)

;; TODO top level set! does not work
(define-syntax (top-interaction-wrapper stx)
  (syntax-parse stx
    #:literals (define require)
    [(_ define form:expr ...) (syntax                               
                               (define form ...))]
    
    [(_ require form:expr ...) (syntax                                
                                (require form ...))]
    
    [(_ form:expr arg:expr) (syntax
                             (begin                               
                               (define type-result (print-type-signature
                                                    (type-check (form arg))))

                               (call-with-values (lambda ()
                                                   (top-interaction form arg))
                                                 (lambda result
                                                   (apply values
                                                          (cons type-result result))))))]

    [(_ . form:expr) (syntax
                      (begin                               
                        (define type-result (print-type-signature
                                             (type-check form)))
                        
                        (call-with-values (lambda ()
                                            (top-interaction . form))
                                          (lambda result
                                            (apply values
                                                   (cons type-result result))))))]))
