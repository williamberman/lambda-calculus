#lang racket/base

(provide reset-bindings-store!
         update-bindings-store!
         print-lambda-abstraction-with-bindings)

(require "core.rkt"
         "app.rkt"
         racket/contract
         racket/match)

(define *bindings-store* (make-hash))

(define (reset-bindings-store!)
  (set! *bindings-store* (make-hash)))

(define bindings? (and/c hash? immutable?))

(define (update-bindings-store! #:prev-term prev-term
                                #:next-term next-term
                                #:bound-value bound-value)
  (when (and (lambda-abstraction? prev-term)
             (lambda-abstraction? next-term))    
    
    (define prev-bindings (hash-ref *bindings-store*
                                    prev-term
                                    (make-bindings)))
  
    (define next-bindings (binding-set #:lambda-abstraction prev-term
                                       #:bound-value bound-value
                                       #:bindings prev-bindings))
  
    (hash-set! *bindings-store* next-term next-bindings)))

(define/contract (make-bindings)
  (-> bindings?)
  (make-immutable-hash))

(define/contract (binding-set #:lambda-abstraction lambda-abstraction
                              #:bound-value bound-value
                              #:bindings bindings)
  (#:lambda-abstraction lambda-abstraction?
   #:bound-value any/c
   #:bindings bindings?
   . -> .
   bindings?)
  (hash-set bindings
            (lambda-abstraction-binding lambda-abstraction)
            bound-value))

(define/contract (print-lambda-abstraction-with-bindings lambda-abstraction)
  (-> lambda-abstraction? list?)
  
  (define bindings (hash-ref *bindings-store* lambda-abstraction (make-bindings)))

  (map-lambda-abstraction-bindings
     #:body (lambda-abstraction-body lambda-abstraction)
     #:bindings bindings
     #:mapper map-binding))

(define (map-binding #:literal literal #:bindings bindings)
  (if (hash-has-key? bindings literal)
      (let [(bound-value (hash-ref bindings literal))]
        (if (lambda-abstraction? bound-value)
            (print-lambda-abstraction-with-bindings bound-value)
            bound-value))
      literal))

(define (map-lambda-abstraction-bindings #:body body #:mapper mapper #:bindings bindings)  
  (match body
    [(list 'lambda binding sub-body)
     (list 'lambda
           binding
           (map-lambda-abstraction-bindings #:body sub-body
                                            #:mapper mapper
                                            #:bindings (hash-remove bindings binding)))]

    [(list vals ...)
     (map (lambda (sub-body)
            (map-lambda-abstraction-bindings #:body sub-body
                                             #:mapper mapper
                                             #:bindings bindings))
          vals)]

    [literal (mapper #:literal literal #:bindings bindings)]))