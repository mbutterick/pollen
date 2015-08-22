#lang racket/base
(provide (all-defined-out))

(define (load-in-namespace to-ns . module-names)
  (for-each (λ(mn) (eval `(require ,mn) to-ns)) module-names))

(define (copy-from-namespace from-ns to-ns . module-names)
  (for-each (λ(mn) (namespace-attach-module from-ns mn to-ns)) module-names))
