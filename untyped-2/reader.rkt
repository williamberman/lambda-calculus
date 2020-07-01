#lang racket/base

(provide lc:assignment-macro
         lc:variable-macro
         lc:application-macro
         lc:abstraction-macro
         lc:native-data-type-macro
         lc->)

(require "core.rkt"
         "repl.rkt"
         [for-syntax syntax/parse racket/base])

(define-syntax (lc-> stx)
  (syntax-parse stx
    [(_ forms:expr ...)
     (syntax
      (repl-eval! (lc->-helper forms ...)))]))

(define-syntax (lc->-helper stx)
  (syntax-parse stx
    [(_ variable:id)
     (syntax
      (lc:variable-macro . variable))]
    [(_ (recipient:expr arguments:expr ...+))     
     (syntax
      (lc->-application-reducer (lc-> recipient) arguments ...))]
    [(_ parsed:expr)     
     (let ([expanded (syntax->datum (local-expand #'parsed 'expression #f))])
       (if (or (string? expanded)
               (number? expanded)
               (symbol? expanded))
           (datum->syntax stx (list 'lc:native-data-type-macro expanded))
           (raise-syntax-error 'lc-> "Not a native data type")))]))

(define-syntax (lc->-application-reducer stx)
  (syntax-parse stx
    [(_ recipient:expr argument:expr)
     (syntax
      (lc:application-macro recipient (lc-> argument)))]
    [(_ recipient:expr argument:expr rest-arguments:expr ...+)
     (syntax
      (lc->-application-reducer (lc:application-macro recipient (lc-> argument))
                                rest-arguments ...))]))

(define-syntax (lc:assignment-macro stx)
  (syntax-parse stx
    [(_ binding:id body:expr ...+)
     (syntax
      (lc:assignment 'binding (list body ...)))]))

(define-syntax (lc:variable-macro stx)
  (syntax-parse stx
    [(_ . binding:id)
     (syntax
      (lc:variable 'binding))]))

(define-syntax (lc:application-macro stx)
  (syntax-parse stx
    [(_ recipient:expr argument:expr)
     (syntax
      (lc:application recipient argument))]
    [(_ recipient:expr argument:expr rest-arguments:expr ...+)
     (syntax
      (lc:application-macro (lc:application-macro recipient argument) rest-arguments ...))]))

(define-syntax (lc:abstraction-macro stx)  
  (syntax-parse stx
    [(_ binding:id body:expr ...+)
     (syntax            
      (lc:abstraction 'binding (list body ...)))]
    [(_ (binding:id) body:expr ...+)
     (syntax
      (lc:abstraction 'binding (list body ...)))]
    [(_ (binding:id rest-bindings:id ...+) body:expr ...+)
     (syntax
      (lc:abstraction 'binding
                      (list (lc:abstraction-macro (rest-bindings ...)
                                                  body ...))))]))

;; TODO does not support symbols
(define-syntax (lc:native-data-type-macro stx)
  (syntax-parse stx
    [(_ . data:expr)
     (syntax
      (lc:native-data-type (#%datum . data)))]))
