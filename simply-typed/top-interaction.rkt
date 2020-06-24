#lang racket/base

(provide (rename-out [top-interaction-wrapper top-interaction]))

(require "type-checker.rkt"
         "utils.rkt"
         "../untyped/top-interaction.rkt"
         [for-syntax syntax/parse racket/base])

;; TODO allow type checking to be turned on or off
(define *type-checking* #t)

;; TODO this doesn't allow define and require
(define-syntax (top-interaction-wrapper stx)
  (syntax-parse stx    
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
                                            (top-interaction form))
                                          (lambda result
                                            (apply values
                                                   (cons type-result result))))))]))
