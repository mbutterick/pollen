#lang at-exp racket/base
(require rackunit racket/runtime-path pollen/render racket/file pollen/setup)

;; define-runtime-path only allowed at top level
(define-runtime-path quick-tour-dir "data/quick-tour/")
(define-runtime-path hello.txt.pp "data/quick-tour/hello.txt.pp")
(define-runtime-path margin.html.pp "data/quick-tour/margin.html.pp")
(define-runtime-path downtown.html.pmd "data/quick-tour/downtown/downtown.html.pmd")
(define-runtime-path uptown.html.pm "data/quick-tour/uptown/uptown.html.pm")

(define-runtime-path hello.txt "data/quick-tour/hello.txt")
(define-runtime-path margin.html "data/quick-tour/margin.html")
(define-runtime-path downtown.html "data/quick-tour/downtown/downtown.html")
(define-runtime-path uptown.html "data/quick-tour/uptown/uptown.html")

(define-runtime-path pollen-cache "data/quick-tour/pollen-cache")
(define-runtime-path pollen-cache-uptown "data/quick-tour/uptown/pollen-cache")
(define-runtime-path pollen-cache-downtown "data/quick-tour/downtown/pollen-cache")

;; test makes sure that quick tour files work 
(parameterize ([current-output-port (open-output-string)]
               [current-directory quick-tour-dir]
               [setup:current-project-root quick-tour-dir])
  (check-not-exn (λ _ (render-to-file-if-needed hello.txt.pp)))
  (check-not-exn (λ _ (render-to-file-if-needed margin.html.pp)))
  (check-not-exn (λ _ (render-to-file-if-needed downtown.html.pmd)))
  (check-not-exn (λ _ (render-to-file-if-needed uptown.html.pm)))
  (check-true (file-exists? hello.txt))
  (check-true (file-exists? margin.html))
  (check-true (file-exists? downtown.html))
  (check-true (file-exists? uptown.html)))

(for-each delete-file (list hello.txt margin.html downtown.html uptown.html))
(for-each delete-directory/files (list pollen-cache pollen-cache-uptown pollen-cache-downtown))