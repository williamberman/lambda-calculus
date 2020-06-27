#lang s-exp "../untyped/lang.rkt"

(require "../untyped/stdlib/stdlib-with-translations.rkt"
         "typings.rkt"
         "core.rkt"
         "utils.rkt"
         "type-checker.rkt"
         (rename-in "top-interaction.rkt"
                    [top-interaction #%top-interaction]))

(define foo (Type-> a String (Type-> Boolean String)))
