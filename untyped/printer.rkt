#lang racket/base

(provide register-printer!
         printer
         set-print-mode!
         print-mode?
         print-with)

(define *print-mode* #f)

(define *printers* (make-hash))

(define-struct printer-definition (id description func)
  #:property prop:procedure (struct-field-index func))

(define (register-printer! id description func)
  (hash-set! *printers* id (make-printer-definition id description func)))

(define (set-print-mode! print-mode)
  (set! *print-mode* print-mode))

(define printer (make-keyword-procedure
                 (lambda (kws kw-args . rest)
                   (keyword-apply (get-printer *print-mode*)
                                  kws
                                  kw-args
                                  rest))))

(define (print-mode?) *print-mode*)

(define (print-with printer-key)
  (make-keyword-procedure
   (lambda (kws kw-args . rest)
     (keyword-apply (get-printer printer-key)
                    kws
                    kw-args
                    rest))))

(define (get-printer printer-key)
  (hash-ref *printers* printer-key
            (lambda () (error 'printer "No printer for ~a" printer-key))))
