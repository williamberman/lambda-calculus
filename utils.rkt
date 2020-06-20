#lang racket/base

(provide tree-map)

(require racket/function)

(define (tree-map pred lst)
  (let loop ((lst lst)
             (acc identity))
    (cond
      ((null? lst)
       (acc '()))
      ((not (pair? (car lst)))
       (loop (cdr lst) (lambda (r)
                         (acc (cons (pred (car lst)) r)))))
      (else
       (loop (cdr lst)
             (lambda (r)
               (acc (cons (loop (car lst) identity) r))))))))