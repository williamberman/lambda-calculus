#lang s-exp "../lang.rkt"

(provide cons
         nil
         nil?
         head
         tail)

(require "boolean.rkt"
         "pair.rkt")

;; Lists are defined in terms of their fold function.
;; '(x y z) -> '(reducer x (reducer y (reducer z nil)))
(define cons (lambda hd
               (lambda tl
                 (lambda cns
                   (lambda nl
                     ((cns hd) ((tl cns) nl)))))))

(define nil (lambda cns
              (lambda nl
                nl)))

(define nil? (lambda lst
               (let ([reducer (lambda _ (lambda _ false))])
                 ((lst reducer) true))))

(define head (lambda lst
               (let ([reducer (lambda hd (lambda _ hd))])
                 ((lst reducer) false))))

(define tail (lambda lst
               (let ([reducer (lambda hd
                                (lambda acc
                                  ((pair (second acc)) ((cons hd) (second acc)))))]
                     [init ((pair nil) nil)])
                 (first ((lst reducer) init)))))

(module+ test
  (require rackunit)
  (define lst ((cons 1) nil))

  (check-equal? (head lst) 1)

  (check-equal? (tail lst) nil)

  (check-equal? false (nil? lst))

  (check-equal? true (nil? nil))

  (check-equal 2 (head (tail (cons 1))))

  )
