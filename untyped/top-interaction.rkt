#lang racket/base

(provide top-interaction
         pretty-printer-description
         pretty-printer)

(require "printer.rkt"
         "translation.rkt"
         "core.rkt"
         "app.rkt"
         "../utils.rkt"
         [for-syntax syntax/parse racket/base])

(define pretty-printer-description
  "If a translator exists for the term, use it to translate the term into a human readable interpretation. Else, fall back to the 'lambda-terms printer.")

(define (pretty-printer term)
  (if (lambda-abstraction? term)
      (if (can-print-readable-translation? term)
          (tree-map pretty-printer (print-readable-translation term))
          ((print-with 'lambda-terms) #:next-term term))
      term))

(define-syntax (top-interaction stx)
  (syntax-parse stx
    #:literals (define require)
    [(_ define form:expr ...) (syntax                               
                               (define form ...))]
    
    [(_ require form:expr ...) (syntax                                
                                (require form ...))]
    
    [(_ form:expr arg:expr) (syntax
                             ;; App must be explicitly added for the outtermost function application
                             
                             (let ([result (app form arg)])
                               (printer #:prev-term form #:next-term result #:bound-value arg)))]

    [(_ . form:expr) (syntax
                      (printer #:next-term form))]))
