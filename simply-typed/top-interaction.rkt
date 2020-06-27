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

(define-syntax (top-interaction-wrapper stx)
  (syntax-parse stx
    #:literals (define require)
    ;; TODO define loses type information
    [(_ define form:expr ...) (syntax                               
                               (define form ...))]
    
    [(_ require form:expr ...) (syntax                                
                                (require form ...))]
    
    [(_ form:expr arg:expr ...+) (syntax                                  
                                  (with-type-check
                                    (form arg ...)
                                    form arg ...))]

    [(_ . form:expr) (syntax                      
                      (with-type-check
                        form
                        form))]))

(define-syntax (with-type-check stx)  
  (syntax-parse stx
    [(_ to-check:expr to-exec:expr ...+)
     (syntax      
      (if (is-type-macro? to-exec ...)
          (displayln (print-type-signature to-exec ...))
          (if *type-checking*
              (with-handlers ([type-exn?
                               (lambda (e) (displayln (exn-message e)))])
                (displayln (print-type-signature (type-check to-check)))
                (top-interaction to-exec ...))
              (top-interaction to-exec ...))))]))

(define-syntax (is-type-macro? stx)
  (syntax-parse stx
    [(_ maybe-type:id)
     (syntax
      (is-type? maybe-type))]
    [(_ maybe-type:expr rest:expr ...)
     (syntax
      (is-type? maybe-type))]))
