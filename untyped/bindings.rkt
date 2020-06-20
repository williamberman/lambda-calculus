#lang racket/base

(provide reset-bindings-store!)

(require "core.rkt"
         "app.rkt"
         racket/contract)

(define *bindings-store* (make-hash))

(define (reset-bindings-store!)
  (set! *bindings-store* (make-hash)))

(define bindings? (and/c hash? immutable?))

(define (update-bindings-store! #:prev-term prev-term #:next-term next-term #:bound-value bound-value)
  (define prev-bindings (hash-ref *bindings-store* prev-term (make-bindings)))
  
  (define next-bindings (binding-set #:lambda-abstraction next-term
                                     #:bound-value bound-value
                                     #:bindings prev-bindings))
  
  (hash-set! *bindings-store* next-term next-bindings))

;; TODO should this side effect thing really go here
(add-app-hook! update-bindings-store!)

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

(define (print-with-bindings #:lambda-abstraction lambda-abstraction #:bindings bindings)
  (map-lambda-abstraction-bindings
   lambda-abstraction
   (lambda (symbol)
     ;; TODO do something with the bound value if it's a lambda-abstraction
     (if (hash-has-key? bindings symbol)
         (let
           [(bound-value (hash-ref bindings symbol))]
           bound-value)
         symbol))))