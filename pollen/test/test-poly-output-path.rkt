#lang at-exp racket/base
(require rackunit racket/runtime-path pollen/render racket/file pollen/setup txexpr xml)

;; define-runtime-path only allowed at top level
(define-runtime-path poly-output-path-dir "data/poly-output-path")
(define-runtime-path pollen.rkt "data/poly-output-path/pollen.rkt")
(define-runtime-path test.poly.pm "data/poly-output-path/test.poly.pm")
(define-runtime-path test.txt "data/poly-output-path/test.txt")
(define-runtime-path test.html "data/poly-output-path/test.html")
(define-runtime-path pollen-cache "data/poly-output-path/compiled")

(parameterize ([current-output-port (open-output-string)]
               [current-directory poly-output-path-dir]
               [current-project-root poly-output-path-dir])

  (for ([parallel? (list #true #false)])
    ;; passing "text.txt" as argument should force use of `txt` rendering
    (render-batch #:parallel parallel? test.txt)
    (check-equal? (file->string test.txt) "(root hello world)")
    (delete-file test.txt)
    (check-false (file-exists? test.html))
    ;; passing poly source as argument should result in default (html) rendering
    (render-batch #:parallel parallel? test.poly.pm)
    (check-txexprs-equal?
     (string->xexpr (file->string test.html))
     (string->xexpr "<html><head><meta charset=\"UTF-8\"/></head><body><root>hello world</root></body></html>"))
    (delete-file test.html)
    (check-false (file-exists? test.txt))))

(delete-directory/files pollen-cache)