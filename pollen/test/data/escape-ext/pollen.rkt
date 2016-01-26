#lang racket/base
(provide (all-defined-out))

(module world racket/base
  (provide (all-defined-out))
  (define compile-cache-active #f)
  (define extension-escape-char #\$))