#lang racket/base
(require (for-syntax racket/base pollen/command))

;; Handle commands from raco

(define-for-syntax args (current-command-line-arguments))
(define-for-syntax arg-command-name (with-handlers ([exn:fail? (λ(exn) #f)]) (vector-ref args 0)))

(define-for-syntax arg-project-directory
  (with-handlers ([exn:fail? (λ(exn) (current-directory))])
    (path->complete-path (simplify-path (string->path (vector-ref args 1))))))

(define-for-syntax (command-error error-string)
  `(displayln (string-append "Error: ", error-string)))

;; we work in syntax layer because 'start' has to require pollen/server,
;; which is slow, and needs to happen at the top level.
(define-syntax (select-syntax-for-command stx)
  (datum->syntax stx 
                 (case arg-command-name
                   [(#f "help") (handle-help)]
                   [("start") (handle-start arg-project-directory)]
                   [else (handle-else arg-command-name)])))

(select-syntax-for-command)


