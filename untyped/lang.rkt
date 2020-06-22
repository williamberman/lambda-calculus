#lang racket/base

(provide set-print-mode!
         print-mode?
         (rename-out [make-lambda-abstraction lambda]
                     [top-interaction #%top-interaction]
                     [app #%app]
                     [lambda proc]
                     [bindings-for-wrapper bindings-for])

         (except-out (all-from-out racket/base)
                     lambda
                     #%top-interaction
                     #%app))

(require "core.rkt"
         "printer.rkt"
         "top-interaction.rkt"
         "app.rkt"
         "bindings.rkt")

(add-app-hook! update-bindings-store!)

(register-printer! 'pretty
                   pretty-printer-description
                   (lambda (#:prev-term [prev-term #f]
                            #:bound-value [bound-value #f]
                            #:next-term next-term)
                     (pretty-printer next-term)))

(register-printer! 'debug
                   "Print the underlying racket data type."
                   (lambda (#:prev-term [prev-term #f]
                            #:bound-value [bound-value #f]
                            #:next-term next-term)
                     next-term))

(register-printer! 'lambda-terms
                   "Print the lambda calculus interpretation of the term."
                   print-lambda-abstraction-with-bindings)

(set-print-mode! 'pretty)

(define (bindings-for-wrapper term)
  (hash-map (bindings-for term) (lambda (key value) (list key (printer value #f)))))


