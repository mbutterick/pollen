#lang racket/base
(require (only-in scribble/reader make-at-reader) pollen/world racket/path pollen/project-requires)

(provide make-reader-with-mode (all-from-out pollen/world))


(define read-inner (make-at-reader 
                    #:command-char world:expression-delimiter 
                    #:syntax? #t 
                    #:inside? #t))


(define (make-custom-read custom-read-syntax-proc) 
  (λ(p)
    (syntax->datum
     (custom-read-syntax-proc (object-name p) p))))


(define (make-custom-read-syntax reader-mode)
  (λ (path-string p)
    (define file-contents (read-inner path-string p))
    (datum->syntax file-contents 
                   `(module pollen-lang-module pollen 
                      (define reader-mode ',reader-mode)
                      (define here-path ,(if (symbol? path-string) (symbol->string path-string) (path->string path-string)))
                      ,(require+provide-project-require-files path-string)
                      ,@file-contents) 
                   file-contents)))


(define-syntax-rule (make-reader-with-mode mode)
  (begin
    (define reader-mode mode)
    (define custom-read-syntax (make-custom-read-syntax reader-mode))
    (define custom-read (make-custom-read custom-read-syntax))
    (provide (rename-out [custom-read read] [custom-read-syntax read-syntax]))))