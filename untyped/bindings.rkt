#lang racket/base

(provide update-bindings-store!
         print-lambda-abstraction-with-bindings
         bindings-for)

(require "core.rkt"
         racket/contract
         racket/match)

(define *bindings-store* (make-weak-hash))

(define (bindings-for term)
  (hash-ref *bindings-store* term (make-bindings)))

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

(define (print-lambda-abstraction-with-bindings #:prev-term [prev-term #f]
                                                #:bound-value [bound-value #f]
                                                #:next-term next-term)
  
  (define printed (if (lambda-abstraction? next-term)
                      (x-print-lambda-abstraction-with-bindings
                       (lambda-abstraction-body next-term)
                       (hash-ref *bindings-store* next-term (make-bindings)))
                      next-term))
  
  (if (lambda-abstraction? prev-term)
      (let ([prev-term-printed (x-print-lambda-abstraction-with-bindings
                                (caddr (lambda-abstraction-body prev-term))
                                (hash-set (hash-ref *bindings-store* prev-term (make-bindings))
                                          (lambda-abstraction-binding prev-term)
                                          bound-value))])
        (if (equal? printed prev-term-printed)
            printed
            (values printed prev-term-printed)))
      printed))

(define/contract (x-print-lambda-abstraction-with-bindings body bindings)
  (-> list? bindings? list?)

  (map-lambda-abstraction-bindings
     #:body body
     #:bindings bindings
     #:mapper map-binding))

(define (map-binding #:literal literal #:bindings bindings)
  (if (hash-has-key? bindings literal)
      (let [(bound-value (hash-ref bindings literal))]
        (if (lambda-abstraction? bound-value)
            (print-lambda-abstraction-with-bindings #:next-term bound-value
                                                    #:prev-term #f
                                                    #:bound-value #f)
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
