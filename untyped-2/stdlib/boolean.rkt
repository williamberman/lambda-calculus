#lang s-exp "../lang.rkt"

(define true (lambda (tru fls)
               tru))

;; (define false (lambda (tru fls)
;;                 fls))

;; (define if (lambda (predicate consequent alternative)
;;              (predicate consequent alternative)))

;; (define and (lambda (a b)
;;               (a b false)))

;; (define or (lambda (a b)
;;              (a true b)))

;; (define not (lambda a
;;               (a false true)))
