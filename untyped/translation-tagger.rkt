#lang racket/base

(provide set-translation-tags*!
         translation-tags-of
         apply-next-tag!
         any)

(require "translation.rkt")

(define *tags* (make-weak-hash))

(define any 'any)

(define set-translation-tags*!
  (lambda (lambda-calculus-term . tags)
    ;; A single tag indicates that this term is not intended to produce any more tags
    ;; and may be marked for translation
    (when (and (= (length tags) 1)
               (not (is-tagged? lambda-calculus-term)))      
      (tag-translation! lambda-calculus-term (car tags)))
    
    (hash-set! *tags* lambda-calculus-term tags)))

(define (translation-tags-of lambda-calculus-term)
  (hash-ref *tags* lambda-calculus-term any))

(define (apply-next-tag! prev-term next-term)
  (when (hash-has-key? *tags* prev-term)
    (define prev-tags (hash-ref *tags* prev-term))
    (when (not (null? prev-tags))
      (define next-tags (cdr prev-tags))
      (apply set-translation-tags*! (cons next-term next-tags)))))
