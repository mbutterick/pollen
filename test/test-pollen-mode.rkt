#lang at-exp racket/base
(require rackunit pollen/world racket/runtime-path)

;; define-runtime-path only allowed at top level
(define-runtime-path test-dir "data/pollen-mode")
(define-runtime-path test-source "data/pollen-mode/test-pollen-mode.foo")

(parameterize ([current-directory test-dir]
               [world:current-project-root test-dir])
  (check-equal? ((dynamic-require test-source 'proc)) "fooXbarXzam"))