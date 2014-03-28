#lang racket/base
(require (only-in scribble/reader make-at-reader) pollen/world racket/path pollen/project)

(provide define+provide-reader-in-mode (all-from-out pollen/world))

(define (make-custom-read custom-read-syntax-proc) 
  (λ(p)
    (syntax->datum
     (custom-read-syntax-proc (object-name p) p))))


(define (make-custom-read-syntax reader-mode)
  (λ (path-string p)
    (define read-inner (make-at-reader 
                    #:command-char (if (or (equal? reader-mode world:mode-template) 
                                                   (regexp-match (pregexp (format "\\.~a$" world:template-source-ext)) path-string))
                                       world:template-command-marker
                                       world:command-marker)
                    #:syntax? #t 
                    #:inside? #t))
    (define file-contents (read-inner path-string p))
    (datum->syntax file-contents 
                   `(module pollen-lang-module pollen 
                      (define reader-mode ',reader-mode)
                      (define reader-here-path ,(cond
                                                  [(symbol? path-string) (symbol->string path-string)]
                                                  [(equal? path-string "unsaved editor") path-string]
                                                  [else (path->string path-string)]))
                      ,(require+provide-project-require-files path-string)
                      ,@file-contents) 
                   file-contents)))


(define-syntax-rule (define+provide-reader-in-mode mode)
  (begin
    (define reader-mode mode)
    (define custom-read-syntax (make-custom-read-syntax reader-mode))
    (define custom-read (make-custom-read custom-read-syntax))
    (provide (rename-out [custom-read read] [custom-read-syntax read-syntax]))))