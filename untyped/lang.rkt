#lang racket/base

(provide set-print-mode!
         print-mode?
         (rename-out [make-lambda-abstraction lambda]
                     [top-interaction #%top-interaction]
                     [app #%app]
                     [lambda proc])

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

(register-printer! 'pretty pretty-printer-description pretty-printer)

(register-printer! 'debug
                   "Print the underlying racket data type."
                   (lambda (lambda-abstraction) lambda-abstraction))

(register-printer! 'lambda-terms
                   "Print the lambda calculus interpretation of the term."
                   print-lambda-abstraction-with-bindings)

(set-print-mode! 'pretty)


