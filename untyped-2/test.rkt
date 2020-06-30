#lang s-exp "lang.rkt"

(define foo (lambda x x))

(define bar (lambda y (y (lambda z z))))
