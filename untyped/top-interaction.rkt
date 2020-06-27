#lang racket/base

(provide top-interaction
         pretty-printer-description
         pretty-printer)

(require "printer.rkt"
         "translation.rkt"
         "core.rkt"
         "app.rkt"
         "../utils.rkt"
         racket/pretty
         [for-syntax syntax/parse racket/base])

(define pretty-printer-description
  "If a translator exists for the term, use it to translate the term into a human readable interpretation. Else, fall back to the 'lambda-terms printer.")

(define (pretty-printer term)
  (if (lambda-abstraction? term)
      (if (can-print-readable-translation? term)
          (tree-map pretty-printer (print-readable-translation term))
          ((print-with 'lambda-terms) #:next-term term))
      term))

;; I don't like when lists print with a quote, so
;; we pretty-display those and return everything else
(define (filter-result result)  
  (if (list? result)
      (begin
        (pretty-display result)
        #f)
      #t))

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
                               (call-with-values (lambda ()
                                                   (printer #:prev-term form
                                                            #:next-term result
                                                            #:bound-value arg))
                                                 (lambda results
                                                   (apply values
                                                          (filter filter-result results))))))]

    [(_ form:expr) (syntax                    
                    (let ([result (printer #:next-term form)])                      
                      (when (filter-result result)
                        result)))]

    [(_ . form:expr) (syntax                    
                    (let ([result (printer #:next-term form)])                      
                      (when (filter-result result)
                        result)))]))
