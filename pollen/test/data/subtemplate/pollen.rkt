#lang racket/base
(require pollen/setup)
(provide (all-defined-out))

(module setup racket/base
  (provide (all-defined-out))
  (define poly-targets '(html txt))
  (define compile-cache-active #f))