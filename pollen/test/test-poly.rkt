#lang at-exp racket/base
(require rackunit
         pollen/setup
         racket/runtime-path
         pollen/render
         racket/file)

;; define-runtime-path only allowed at top level
(define-runtime-path poly-dir "data/poly")
(define-runtime-path poly-source "data/poly/test.poly.pm")
(define-runtime-path pollen-cache "data/poly/compiled")
(define-runtime-path test.txt "data/poly/test.txt")
(define-runtime-path test.html "data/poly/test.html")

(parameterize ([current-directory poly-dir]
               [current-project-root poly-dir]
               [current-output-port (open-output-string)])
  (parameterize ([current-poly-target 'txt])
    (check-equal? (render poly-source) "TITLE is **big**"))
  (parameterize ([current-poly-target 'html])
    (check-equal? (render poly-source) (format "~v" '(root (h2 "title") " is " (strong "big"))))))

(parameterize ([current-output-port (open-output-string)]
               [current-directory poly-dir]
               [current-project-root poly-dir])

  ;; make sure that batch works with multiple output files
  ;; related to one poly sourc
  ;; or duplicate output files (which will only be rendered once)
  (for ([parallel? (list #true #false)])
    (render-batch #:parallel parallel? test.html test.txt test.html)
    (check-equal? (file->string test.txt) "TITLE is **big**")
    (check-equal? (file->string test.html) (format "~v" '(root (h2 "title") " is " (strong "big"))))
    (delete-file test.txt)
    (delete-file test.html)))

(delete-directory/files pollen-cache)