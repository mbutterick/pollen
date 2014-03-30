#lang racket/base
(require pollen/main-base)
(define+provide-module-begin-in-mode world:mode-template)

(module reader racket/base
  (require pollen/reader-base)
  (define+provide-reader-in-mode world:mode-template))