#lang racket/base
(require pollen/world)
(provide (all-defined-out))

(module world racket/base
  (provide (all-defined-out))
  (define poly-targets '(html txt))
  (define compile-cache-active #f))