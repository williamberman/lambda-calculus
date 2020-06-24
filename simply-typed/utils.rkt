#lang racket/base

(require "core.rkt"
         racket/string)

(provide print-type-signature)

(define (print-type-signature type)
  (if (list? type)
      (cond [(null? type) Unit]
            [(null? (cdr type)) (car type)]
            [else (string-join
                   (map (lambda (a-type)                          
                          (format "~a" (if (type-variable? a-type)
                                           (type-variable-symbol a-type)
                                           a-type)))
                        type)
                   " -> ")])
      type))
