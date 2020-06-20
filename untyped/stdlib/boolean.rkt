#lang s-exp "../lang.rkt"

(provide true
         false
         if
         and
         or
         not)

;; Church booleans

(define true (lambda tru
               (lambda fls
                 tru)))

(define false (lambda tru
                (lambda fls
                  fls)))

(define if (lambda predicate
             (lambda consequent
               (lambda alternative
                 ((predicate consequent) alternative)))))

(define and (lambda a
              (lambda b
                ((a b) false))))

(define or (lambda a
             (lambda b
               ((a true) b))))

(define not (lambda a
              ((a false)
               true)))

(module+ test
  (require rackunit)
  (check-equal? 'consequent (((if true) 'consequent) 'alternative))
  (check-equal? 'alternative (((if false) 'consequent) 'alternative))
  
  (check-equal? true ((and true) true))
  (check-equal? false ((and true) false))
  (check-equal? false ((and false) true))
  (check-equal? false ((and false) false))

  (check-equal? true ((or true) true))
  (check-equal? true ((or  true) false))
  (check-equal? true ((or  false) true))
  (check-equal? false ((or  false) false))

  (check-equal? true (not false))
  (check-equal? false (not true)))
