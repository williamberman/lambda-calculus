#lang s-exp "../untyped/lang.rkt"

(require "../untyped/stdlib/stdlib-with-translations.rkt"
         "typings.rkt"
         (rename-in "top-interaction.rkt"
                    [top-interaction #%top-interaction]))
