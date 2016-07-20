#lang racket/base
(require rackunit racket/runtime-path)
(define-runtime-path meta-test "data/metas/metatest.html.pm")
(define metas (dynamic-require `(submod ,meta-test metas) 'metas))
(check-equal? (hash-ref metas 'foo) 42)