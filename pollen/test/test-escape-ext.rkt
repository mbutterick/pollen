#lang at-exp racket/base
(require rackunit racket/port racket/system racket/runtime-path compiler/find-exe racket/file)

;; define-runtime-path only allowed at top level
(define-runtime-path test-dir "data/escape-ext")
(define-runtime-path test-file "data/escape-ext/test_html.pp")
(define-runtime-path result-file "data/escape-ext/test.html")

;; `find-exe` avoids reliance on $PATH of the host system
(define racket-path (find-exe))
(define-values (raco-dir file _) (split-path racket-path))
(when racket-path
  (define raco-path (build-path raco-dir "raco"))
  (define (render path)
    ;; need to cd first to pick up directory require correctly
    (define cmd-string (format "cd '~a' ; '~a' pollen render '~a'" test-dir raco-path path))
    (parameterize ([current-error-port (open-output-nowhere)])
      (system cmd-string)))
  (when (file-exists? result-file)
    (delete-file result-file))
  (render test-file)
  (check-true (file-exists? result-file))
  (check-equal? (file->string result-file) "test")
  (delete-file result-file))
