#lang at-exp racket/base
(require rackunit
         racket/runtime-path
         pollen/file)

(define-runtime-path example "data/ext/example.html")
(define-runtime-path example-pm "data/ext/example.html.pm")
(define-runtime-path another "data/ext/sub/another.html")
(define-runtime-path another-pm "data/ext/sub/another.html.pm")

(check-equal? (get-markup-source example) example-pm)
(check-equal? (get-markup-source another) another-pm)
