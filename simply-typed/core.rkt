#lang racket/base

(provide define-base-type
         :
         define-lc-type
         Unit
         unit
         Any         
         find-base-type
         type-variable?
         type-variable-symbol
         get-type-tag
         base-type?
         base-type-predicate
         base-type-identifier
         function-signature?
         lc-type?
         lc-type-identifier
         lc-type-constraints)

(require [for-syntax syntax/parse racket/base]
         data/gvector)

(define-struct base-type (identifier predicate))

(define-struct type-variable (symbol))

(define-struct lc-type (identifier constraints apply-constraints)
  #:property prop:procedure (struct-field-index apply-constraints)

  ;; Ignore apply-constraints for equality.
  ;; https://docs.racket-lang.org/guide/define-struct.html#%28part._struct-equal%29
  #:methods
  gen:equal+hash  
  [(define (equal-proc a b equal?-recur)
     (and (equal?-recur (lc-type-identifier a) (lc-type-identifier b))
          (equal?-recur (lc-type-constraints a) (lc-type-constraints b))))
   
   (define (hash-proc a hash-recur)
     (+ (hash-recur (lc-type-identifier a))
        (* 3 (hash-recur (lc-type-constraints a)))))
   
   (define (hash2-proc a hash2-recur)
     (+ (hash2-recur (lc-type-identifier a))
        (hash2-recur (lc-type-constraints a))))])

(define (make-lc-type-wrapper identifier constraints)
  (define the-lc-type #f)

  (define (apply-constraints . new-constraints)
    (define (helper current-constraints new-constraints)
      (if (null? constraints)
          current-constraints
          (let* ([looking-for (car constraints)]
                 [new-constraints (cdr new-constraints)]
                 [type-variable-found #f]
                 [current-constraints (map (lambda (type)
                                             (if (and (not type-variable-found)
                                                      (equal? type looking-for))

                                                 (begin
                                                   (set! type-variable-found #t)
                                                   looking-for)

                                                 type))
                                           
                                           current-constraints)])
            
            (if type-variable-found
                (helper current-constraints
                        new-constraints)
                (raise "TODO something about trying to apply too many type variables")))))
    
    (make-lc-type-wrapper identifier
                          (helper (lc-type-constraints the-lc-type) new-constraints)))
  
  (set! the-lc-type (make-lc-type identifier constraints apply-constraints))
  
  the-lc-type)

(define *type-checkers* (make-gvector))

(define-syntax (define-base-type stx)
  (syntax-parse stx
    [(_ identifier:id predicate:expr)
     (syntax
      (begin        
        (check-type-identifier! 'identifier)
        (define identifier (make-base-type 'identifier predicate))
        (gvector-add! *type-checkers*
                      identifier)))]))

(define (check-type-identifier! identifier)
  (define first-char (string-ref (symbol->string identifier) 0))
  (when (char-lower-case? first-char)
    (error 'define-base-type "Type identifiers must start with an uppercase letter")))

(define-base-type Any (lambda (term) #t))
(define-base-type Unit (lambda (term) (eq? term unit)))
(define unit (gensym)) ;; TODO probably a better way to do this than doing a gensym

(define-syntax (define-lc-type stx)
  (syntax-parse stx
    [(_ identifier:id constraints:id ...)
     (syntax
      (begin
       (check-type-identifier! 'identifier)
       (define identifier (make-lc-type-wrapper 'identifier (list constraints ...)))))]))

(define *type-tags* (make-weak-hash))

;; TODO shouldn't just blindly apply the type. Should have some way to check if
;; this is an acceptable type to apply
(define-syntax (: stx)
  (syntax-parse stx
    [(_ term:id typing:id ...)
     (syntax
      (hash-set! *type-tags* term (process-typings typing ...)))]))

(define-syntax (process-typings stx)
  (syntax-parse stx
    [(_ typing:id)     
     (if (char-lower-case? (string-ref (symbol->string (syntax->datum #'typing)) 0))
         (syntax
          (type-variable 'typing))
         (syntax
          typing))]
    [(_ typing:id rest-typing:id ...)
     (syntax
      (let ([processed-rest (process-typings rest-typing ...)])
        (cons (process-typings typing) (if (list? processed-rest)
                                           processed-rest
                                           (list processed-rest)))))]))

(define (function-signature? type)
  (list? type))

(define (get-type-tag term)
  (hash-ref *type-tags* term Any))

(define (find-base-type term)
  (findf (lambda (type-checker)
           ((base-type-predicate type-checker) term))
         ;; TODO probably need a better prioritization method
         ;; Reverse so that Any is the last type checked
         (reverse (gvector->list *type-checkers*))))
