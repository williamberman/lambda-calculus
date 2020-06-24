#lang racket/base

(provide define-base-type
         type-check         
         :
         define-lc-type
         Unit
         unit
         print-type-signature
         (rename-out [type-variable-helper type-variable]))

(require [for-syntax syntax/parse racket/base]
         data/gvector
         "../untyped/core.rkt"
         racket/string)

(define *type-checkers* (make-gvector))

(define-struct type-checker (predicate checker))

(define-syntax (define-base-type stx)
  (syntax-parse stx
    [(_ identifier:id predicate:expr)
     (syntax
      (begin
        (define identifier 'identifier)
        (gvector-add! *type-checkers*
                      (make-type-checker predicate
                                         (lambda args 'identifier)))))]))

(define-syntax (define-lc-type stx)
  (syntax-parse stx
    [(_ identifier:id)
     (syntax
      (define identifier 'identifier))]))

(define-base-type Any (lambda (term) #t))
(define-base-type Unit (lambda (term) (eq? term unit)))
(define unit (gensym))

;; TODO this is likely much too slow
(define (type-check term)
  (if (lambda-abstraction? term)
      ;; TODO needs to actually run this check
      (hash-ref *type-tags* term Any)      
      (let ([type-checker (findf (lambda (type-checker)
                                   ((type-checker-predicate type-checker) term))
                                 ;; Reverse so that Any is the last type checked
                                 (reverse (gvector->list *type-checkers*)))])

        (if type-checker
            ((type-checker-checker type-checker) term)
            Any))))

(define (print-type-signature type)
  (if (list? type)
      (cond [(null? type) Unit]
            [(null? (cdr type)) (car type)]
            [else (string-join
                   (map (lambda (a-type)
                          ;; TODO this format should properly format type-variables
                          (format "~a" a-type))
                        type)
                   " -> ")])
      type))

(define *type-tags* (make-weak-hash))

;; TODO this should probably be syntax which checks
;; for unbound terms in typings and makes them type variables
(define (: term . typings)
  (hash-set! *type-tags* term typings))

(define-syntax (type-variable-helper stx)
  (syntax-parse stx
    [(_ identifier:id)
     (syntax
      (make-type-variable 'identifier))]))

(define-struct type-variable (symbol) #:transparent)

;; (define-syntax (define-type stx)
;;   (syntax-parse stx
;;     ;; TODO parameters:<what>
;;     [(_ identifier:id parameters ...)
;;      (syntax
;;       (define identifier (make-derived-type 'identifier
;;                                             (list parameters ...)
;;                                             (lambda args
;;                                               "TODO"))))]))



;; (define-struct derived-type (identifier parameters type-constructor)
;;   #:property prop:procedure (struct-field-index type-constructor))



;; (define (type-of term)
;;   "TODO")

;; (define (type-check term)
;;   "TODO")
