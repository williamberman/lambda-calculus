#lang racket/base

(require "core.rkt"
         racket/string
         racket/contract)

(provide print-type-signature)

(define (print-type-signature type [nested #f])
  (if (list? type)
      ;; TODO this probably isn't actually the unit type
      (cond [(null? type) (type-signature->string Unit)]
            [(null? (cdr type)) (type-signature->string (car type))]
            [else (string-join
                   (map (lambda (a-type)
                          (format "~a" (print-type-signature a-type)))
                        type)
                   " -> ")])
      (type-signature->string type)))

(define/contract (type-signature->string type)
  (any/c . -> . string?)
  (cond [(type-variable? type) (symbol->string (type-variable-symbol type))]
        [(lc-type? type) (if (null? (lc-type-constraints type))
                             (symbol->string (lc-type-identifier type))
                             (format "(~a ~a)"
                                     (lc-type-identifier type)
                                     (string-join (map print-type-signature
                                                       (lc-type-constraints))
                                                  " ")))]
        [(base-type? type) (symbol->string (base-type-identifier type))]
        [else (error 'type-signature->string "Unknown type ~a" type)]))
