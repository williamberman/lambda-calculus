#lang racket/base

(provide define-base-type
         :
         define-lc-type
         Unit
         unit
         Any         
         find-type-checker         
         type-variable?
         type-variable-symbol
         get-type-tag
         type-checker-predicate
         type-checker-identifier)

(require [for-syntax syntax/parse racket/base]
         data/gvector
         racket/string)

(define-struct type-checker (predicate identifier))

(define-struct type-variable (symbol))

(define *type-checkers* (make-gvector))

(define-syntax (define-base-type stx)
  (syntax-parse stx
    [(_ identifier:id predicate:expr)
     (syntax
      (begin
        ;; TODO ensure upper case
        (define identifier 'identifier)
        (gvector-add! *type-checkers*
                      (make-type-checker predicate
                                         'identifier))))]))

(define-base-type Any (lambda (term) #t))
(define-base-type Unit (lambda (term) (eq? term unit)))
(define unit (gensym)) ;; TODO probably a better way to do this than doing a gensym

(define-syntax (define-lc-type stx)
  (syntax-parse stx
    [(_ identifier:id)
     (syntax
      (define identifier 'identifier))]))

(define *type-tags* (make-weak-hash))

(define-syntax (: stx)
  (syntax-parse stx
    [(_ term:id typing:id ...)
     (syntax
      (hash-set! *type-tags* term (process-typings typing ...)))]))

(define-syntax (process-typings stx)
  (syntax-parse stx
    [(_ typing:id)
     (if (char-lower-case? (string-ref (symbol->string 'typing) 0))
         (syntax
          (type-variable 'typing))
         (syntax
          typing))]
    [(_ typing:id rest-typing:id ...)
     (syntax
      (cons (process-typings typing) (process-typings rest-typing ...)))]))

(define (get-type-tag term)
  (hash-ref *type-tags* term Any))

(define (find-type-checker term)
  (findf (lambda (type-checker)
           ((type-checker-identifier type-checker) term))
         ;; TODO probably need a prioritization method
         ;; Reverse so that Any is the last type checked
         (reverse (gvector->list *type-checkers*))))
