#lang at-exp racket/base
(require rackunit pollen/setup racket/runtime-path pollen/render)

;; define-runtime-path only allowed at top level
(define-runtime-path subtemplate-dir "data/subtemplate")
(define-runtime-path one-source "data/subtemplate/one.txt.pm")
(define-runtime-path two-source "data/subtemplate/subdir/two.txt.pm")
(define-runtime-path three-source "data/subtemplate/subdir/subsubdir/three.txt.pm")

(parameterize ([current-directory subtemplate-dir]
               [current-project-root subtemplate-dir]
               [current-output-port (open-output-string)])
  (check-equal? (render one-source) "one in main template")
  (check-equal? (render two-source) "two in main template")
  (check-equal? (render three-source) "three in subsubdir template"))