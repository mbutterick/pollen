#lang racket/base
(require "world.rkt")
(provide (all-defined-out))

(define (project-require-file? path)
  (define path-string (path->string path))
  (equal? (substring path-string (- (string-length path-string) 3) (string-length path-string)) "rkt"))

;; list of all eligible requires in project require directory
(define (get-project-require-files)
  (define extras-directory (build-path (world:current-project-root) world:extras-dir))
  (and (directory-exists? extras-directory)
       ;; #:build? option returns complete paths (instead of just file names)
       (let ([files (filter project-require-file? (directory-list extras-directory #:build? #t))])
         (and (not (equal? '() files)) files))))

(module+ main
(parameterize ([world:current-project-root (string->path "/Users/mb/git/bpt/")])
  (get-project-require-files)))