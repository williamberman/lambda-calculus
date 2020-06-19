#lang racket/base                       ;

(provide register-printer!
         printer
         set-print-mode!
         print-mode?
         print-with)

(require "core.rkt")

(define *print-mode* 'lambda-terms)

(define *printers* (make-hash))

(define-struct printer-definition (id description func)
  #:property prop:procedure (struct-field-index func))

(define (register-printer! id description func)
  (hash-set! *printers* id (make-printer-definition id description func)))

(define (set-print-mode! print-mode)
  (set! *print-mode* print-mode))

(define (printer lambda-abstraction)
  ((hash-ref *printers* *print-mode*) lambda-abstraction))

(define (print-mode?) *print-mode*)

(define (print-with printer-key lambda-abstraction)
  ((hash-ref *printers* printer-key) lambda-abstraction))

(register-printer! 'debug
                   "Print the underlying racket data type."
                   (lambda (lambda-abstraction) lambda-abstraction))

(register-printer! 'lambda-terms
                   "Print the lambda calculus interpretation of the term."
                   print-lambda-abstraction)
