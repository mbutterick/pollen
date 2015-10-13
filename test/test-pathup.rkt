#lang at-exp racket/base
(require rackunit racket/runtime-path pollen/project pollen/render)

(define-runtime-path pathup-one "data/pathup/subdir/test-pathup-one.html.pm")
(define-runtime-path dr-top "data/pathup/pollen.rkt")
(define-runtime-path pathup-two "data/pathup/subdir/subdir/test-pathup-two.html.pm")
(define-runtime-path dr-sub "data/pathup/subdir/subdir/pollen.rkt")
(define-runtime-path template "data/pathup/subdir/template.html")

(check-false (get-directory-require-files "test-pathup.rkt"))
(check-equal? (get-directory-require-files pathup-one) (list dr-top))
(check-equal? (get-directory-require-files pathup-two) (list dr-sub))
(check-equal? (get-template-for pathup-one) template)
(check-equal? (get-template-for pathup-two) template)