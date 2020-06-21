#lang racket/base

(provide top-interaction
         pretty-printer-description
         pretty-printer)

(require "printer.rkt"
         "translation.rkt"
         "core.rkt"
         "bindings.rkt"
         "app.rkt"
         [for-syntax syntax/parse racket/base])

(define pretty-printer-description
  "If a translator exists for the term, use it to translate the term into a human readable interpretation. Else, fall back to the 'lambda-terms printer.")

;; TODO this should recursively walk the result
(define (pretty-printer term)
  (if (can-print-readable-translation? term)
      (print-readable-translation term)
      (print-with 'lambda-terms term)))

;; TODO this should return multiple values for when something
;; entirely evaluates. The first return value will be the
;; result, the second will be the formatted lambda term.
;; Maybe we handle the formatted lambda term in a different location because
;; this is agnostic to the printer for lambda terms
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
                           ;; TODO explain why explicitly calling app
                           (printer-wrapper (app form ...))))]

    [(_ . form:expr) (syntax
                      (begin
                        (reset-bindings-store!)                     
                        (printer-wrapper form)))]))
