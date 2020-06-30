#lang racket/base

(provide (all-defined-out))

(struct lc:abstraction (binding body))
(struct lc:assignment (binding body))
(struct lc:application (recipient argument))
(struct lc:variable (binding))




