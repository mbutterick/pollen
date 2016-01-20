#lang at-exp racket/base
(require rackunit racket/runtime-path pollen/project pollen/render racket/file)

(define-runtime-path pathup-one "data/pathup/subdir/test-pathup-one.html.pm")
(define-runtime-path dr-top "data/pathup/pollen.rkt")
(define-runtime-path pathup-two "data/pathup/subdir/subdir/test-pathup-two.html.pm")
(define-runtime-path dr-sub "data/pathup/subdir/subdir/pollen.rkt")
(define-runtime-path template "data/pathup/subdir/template.html")

(define-runtime-path cache-dir "data/pathup/subdir/pollen-cache")
(define-runtime-path other-cache-dir "data/pathup/subdir/subdir/pollen-cache")

(check-false (get-directory-require-files "test-pathup.rkt"))
(check-equal? (get-directory-require-files pathup-one) (list dr-top))
(check-equal? (get-directory-require-files pathup-two) (list dr-sub))
(check-equal? (get-template-for pathup-one) template)
(check-equal? (get-template-for pathup-two) template)

(when (directory-exists? cache-dir)
  (delete-directory/files cache-dir))

(when (directory-exists? other-cache-dir)
  (delete-directory/files other-cache-dir))