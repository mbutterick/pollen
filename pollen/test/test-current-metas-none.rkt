#lang racket/base
(require pollen/core "pollen.rkt" rackunit)
(check-equal? (test-current-metas) "false")