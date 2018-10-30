#lang at-exp racket/base
(require rackunit pollen/setup racket/runtime-path pollen/render)

;; define-runtime-path only allowed at top level
(define-runtime-path whitespace-dir "data/whitespace")
(define-runtime-path whitespace-source "data/whitespace/whitespace-test.txt.pp")

(parameterize ([current-directory whitespace-dir]
               [current-project-root whitespace-dir]
               [current-output-port (open-output-string)])
  (check-equal? (render whitespace-source) "\n\n\n\n\n\n\n\n\none"))