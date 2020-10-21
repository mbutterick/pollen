#lang at-exp racket/base
(require rackunit racket/runtime-path pollen/render racket/file pollen/setup)

;; define-runtime-path only allowed at top level
(define-runtime-path dir "data/pagetree-output")
(define-runtime-path index.ptree "data/pagetree-output/index.ptree")
(define-runtime-path foo.txt.pp "data/pagetree-output/foo.txt.pp")
(define-runtime-path foo.txt "data/pagetree-output/foo.txt")
(define-runtime-path bar.txt.pp "data/pagetree-output/bar.txt.pp")
(define-runtime-path bar.txt "data/pagetree-output/bar.txt")
(define-runtime-path pollen-cache "data/pagetree-output/compiled")

(parameterize ([current-output-port (open-output-string)]
               [current-directory dir]
               [current-project-root dir])

  ;; passing "index.ptree" as argument should work
  (for ([parallel? (list #true #false)])
       (render-batch #:parallel parallel? index.ptree)    
       (check-true (file-exists? foo.txt))
       (check-equal? (file->string foo.txt) "this is foo")
       (delete-file foo.txt)
       (check-true (file-exists? bar.txt))
       (check-equal? (file->string bar.txt) "this is bar")
       (delete-file bar.txt)))

(delete-directory/files pollen-cache)