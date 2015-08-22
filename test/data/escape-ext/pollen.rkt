#lang racket/base
(provide (all-defined-out))

(module config racket/base
  (provide (all-defined-out))
  (define compile-cache-active #f)
  (define extension-escape-char #\$))