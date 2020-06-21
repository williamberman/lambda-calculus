#lang racket/base

(provide top-interaction
         pretty-printer-description
         pretty-printer)

(require "printer.rkt"
         "translation.rkt"
         "core.rkt"
         "bindings.rkt"
         [for-syntax syntax/parse racket/base])

(define pretty-printer-description
  "If a translator exists for the term, use it to translate the term into a human readable interpretation. Else, fall back to the 'lambda-terms printer.")

(define (pretty-printer term)
  (if (can-print-readable-translation? term)
      (print-readable-translation term)
      (print-with 'lambda-terms term)))

;; TODO this should recursively walk the result
(define (printer-wrapper output)
  (if (lambda-abstraction? output)
      (printer output)
      output))

(define-syntax (top-interaction stx)
  (syntax-parse stx
    #:literals (define require)
    [(_ define form:expr ...) (syntax                               
                               (define form ...))]
    
    [(_ require form:expr ...) (syntax                                
                                (require form ...))]
    
    [(_ form:expr ...+) (syntax
                         (begin
                           (reset-bindings-store!)
                           (define result (printer-wrapper (form ...)))
                           (displayln *bindings-store*)
                           result))]

    [(_ . form:expr) (syntax
                      (begin
                        (reset-bindings-store!)                     
                        (printer-wrapper form)))]))
