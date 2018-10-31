#lang at-exp racket/base
(require rackunit racket/runtime-path pollen/render racket/file pollen/setup)

(define-runtime-path pixel-dir "data/pixel")
(define-runtime-path test-pixel-src "data/pixel/test-pixel.png.pm")
(define-runtime-path test-pixel "data/pixel/test-pixel.png")
(define-runtime-path pixel "data/pixel/pixel.png")
(define-runtime-path template "data/pixel/template.png")

;; test makes sure that quick tour files work 
(parameterize ([current-output-port (open-output-string)]
               [current-directory pixel-dir]
               [current-project-root pixel-dir])
  (check-not-exn (λ _ (render-to-file-if-needed test-pixel-src)))
  (check-true (file-exists? test-pixel))
  (check-equal? (file->bytes test-pixel) (file->bytes pixel)))

(for-each (λ (f) (when (file-exists? f) (delete-file f))) (list test-pixel template))