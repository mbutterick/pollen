#lang racket/base

;; This will prevent tests from littering their directory
;; with pollen-cache files.
(module setup racket/base
  (provide (all-defined-out))
  (define compile-cache-active #f)
  (define render-cache-active #f))