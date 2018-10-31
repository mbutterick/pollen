#lang at-exp racket/base
(require rackunit
         pollen/setup
         racket/runtime-path
         pollen/render)

;; define-runtime-path only allowed at top level
(define-runtime-path poly-dir "data/poly")
(define-runtime-path poly-source "data/poly/test.poly.pm")

(parameterize ([current-directory poly-dir]
               [current-project-root poly-dir]
               [current-output-port (open-output-string)])
  (parameterize ([current-poly-target 'txt])
    (check-equal? (render poly-source) "TITLE is **big**"))
  (parameterize ([current-poly-target 'html])
    (check-equal? (render poly-source) (format "~v" '(root (h2 "title") " is " (strong "big"))))))