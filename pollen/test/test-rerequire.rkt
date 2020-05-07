#lang at-exp racket/base
(require rackunit racket/runtime-path pollen/render racket/file racket/system pollen/setup)

;; define-runtime-path only allowed at top level
(define-runtime-path rerequire-dir "data/rerequire")
(define-runtime-path pollen.rkt "data/rerequire/pollen.rkt")
(define-runtime-path pre.txt.pp "data/rerequire/pre.txt.pp")
(define-runtime-path pre.txt "data/rerequire/pre.txt")
(define-runtime-path template.txt "data/rerequire/template.txt")
(define-runtime-path markup.txt.pm "data/rerequire/markup.txt.pm")
(define-runtime-path markup.txt "data/rerequire/markup.txt")
(define-runtime-path pollen-cache "data/rerequire/compiled")

(copy-file markup.txt.pm pre.txt.pp #t)

;; test makes sure that file render changes after pollen.rkt changes
(parameterize ([current-output-port (open-output-string)]
               [current-directory rerequire-dir]
               [current-project-root rerequire-dir]
               [current-session-interactive? #true])
  
  (display-to-file @string-append{#lang racket/base
 (provide id)
 (define (id) "first")} pollen.rkt #:exists 'replace)
  
  (render-to-file-if-needed markup.txt.pm)
  (check-equal? (file->string markup.txt) "first")
  (render-to-file-if-needed pre.txt.pp)
  (check-equal? (file->string pre.txt) "first")
  
  (sleep 1)
  (display-to-file @string-append{#lang racket/base
 (provide id)
 (define (id) "second")} pollen.rkt #:exists 'replace)
  
  (render-to-file-if-needed markup.txt.pm)
  (check-equal? (file->string markup.txt) "second")
  (render-to-file-if-needed pre.txt.pp)
  (check-equal? (file->string pre.txt) "second"))

(delete-file pre.txt.pp)
(delete-file pre.txt)
(delete-file markup.txt)

(delete-directory/files pollen-cache)