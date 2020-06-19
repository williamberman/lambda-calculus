#lang racket/base

(provide top-interaction)

(require "printer.rkt"
         "translation.rkt"
         "core.rkt"
         [for-syntax syntax/parse racket/base])

(register-printer! 'pretty
                   "If a translator exists for the term, use it to translate the term into a human readable interpretation. Else, fall back to the 'lambda-terms printer."
                   (lambda (term)
                     (if (can-print-readable-translation? term)
                         (print-readable-translation term)
                         (print-with 'lambda-terms term))))

(set-print-mode! 'pretty)

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
                         (printer-wrapper (form ...)))]

    [(_ . form:expr) (syntax
                      (printer-wrapper form))]))
