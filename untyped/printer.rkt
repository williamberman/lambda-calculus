#lang racket/base                       ;

(provide register-printer!
         printer
         set-print-mode!
         print-mode?
         print-with)

(require "core.rkt")

(define *print-mode* #f)

(define *printers* (make-hash))

(define-struct printer-definition (id description func)
  #:property prop:procedure (struct-field-index func))

(define (register-printer! id description func)
  (hash-set! *printers* id (make-printer-definition id description func)))

(define (set-print-mode! print-mode)
  (set! *print-mode* print-mode))

(define (printer lambda-abstraction)
  (if (hash-has-key? *printers* *print-mode*)
      ((hash-ref *printers* *print-mode*) lambda-abstraction)
      (error 'printer "No printer for ~a" *print-mode*)))

(define (print-mode?) *print-mode*)

(define (print-with printer-key lambda-abstraction)
  ((hash-ref *printers* printer-key) lambda-abstraction))