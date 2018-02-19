#lang racket/base
(require racket/string pollen/core)

;; This will prevent tests from littering their directory
;; with pollen-cache files.
(module setup racket/base
  (provide (all-defined-out))
  (define compile-cache-active #f)
  (define render-cache-active #f))

(provide test-current-metas)
(define (test-current-metas)
  (if (current-metas)
      (string-join (sort (map symbol->string (hash-keys (current-metas))) string<?) " ")
      "false"))