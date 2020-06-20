#lang s-exp "../lang.rkt"

(provide pair first second)

(require "boolean.rkt")

(define pair (lambda fst
               (lambda snd
                 (lambda choose
                   ((choose fst) snd)))))

(define first (lambda the-pair
                (the-pair true)))

(define second (lambda the-pair
                 (the-pair false)))

(module+ test
  (require rackunit)

  (define par ((pair 1) 2))

  (check-equal? 1 (first par))
  (check-equal? 2 (second par)))