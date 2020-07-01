#lang typed/racket

(provide (all-defined-out))

(define-type LCTerm (U lc:abstraction
                       lc:assignment
                       lc:application
                       lc:variable
                       lc:native-data-type))

(struct lc:abstraction ([binding : Symbol] [body : (Listof LCTerm)]))
(struct lc:assignment ([binding : Symbol] [body : (Listof LCTerm)]))
(struct lc:application ([recipient : LCTerm] [argument : LCTerm]))
(struct lc:variable ([binding : Symbol]))
(struct lc:native-data-type ([data : (U Number String Symbol)]))
