#lang racket/base

(require "core.rkt"
         racket/string
         racket/contract)

(provide print-type-signature
         is-type?)

(define (is-type? maybe-type)
  (or (type-variable? maybe-type)
      (lc-type? maybe-type)
      (base-type? maybe-type)
      (and (list? maybe-type)
           (andmap is-type? maybe-type))))

;; TODO Deal with nested type parameters
(define (print-type-signature type)  
  (if (list? type)
      ;; TODO this probably isn't actually the unit type
      (cond [(null? type) (type-signature->string Unit)]
            [(null? (cdr type)) (type-signature->string (car type))]
            [else (string-join
                   (map (lambda (x-type)
                          (define printed (print-type-signature x-type))
                          (cond [(and (list? x-type)
                                      ((not (null? (cdr x-type)))))
                                 (wrap-in-parens printed)]
                                [else printed]))
                        type)
                   " -> ")])
      (type-signature->string type)))

(define/contract (type-signature->string type)
  (any/c . -> . string?)
  (cond [(type-variable? type) (symbol->string (type-variable-symbol type))]
        [(lc-type? type) (if (null? (lc-type-constraints type))
                             (symbol->string (lc-type-identifier type))
                             (format "~a ~a"
                                     (lc-type-identifier type)
                                     (string-join (map (lambda (x-type)
                                                         (define printed (print-type-signature x-type))
                                                         (cond [(and (list? x-type)
                                                                     ((not (null? (cdr x-type)))))
                                                                (wrap-in-parens printed)]
                                                               [(and (lc-type? x-type)
                                                                     (not (null? (lc-type-constraints x-type))))
                                                                (wrap-in-parens printed)]
                                                               [else printed]))
                                                       (lc-type-constraints type))
                                                  " ")))]
        [(base-type? type) (symbol->string (base-type-identifier type))]
        [else (error 'type-signature->string "Unknown type ~a" type)]))

(define (wrap-in-parens str)
  (format "(~a)" str))
