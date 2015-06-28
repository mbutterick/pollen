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
                                               (and (string? path-string) (regexp-match (pregexp (format "\\.~a$" (world:get-template-source-ext))) path-string)))
                                           (world:get-template-command-char)
                                           (world:get-command-char))
                        #:syntax? #t 
                        #:inside? #t))
    (define file-contents (read-inner path-string p))
    (datum->syntax file-contents 
                   `(module repl-wrapper racket/base
                      (module pollen-lang-module pollen 
                        (define reader-mode ',reader-mode)
                        (define reader-here-path ,(cond
                                                    [(symbol? path-string) (symbol->string path-string)]
                                                    [(equal? path-string "unsaved editor") path-string]
                                                    [else (path->string path-string)]))
                        (define parser-mode
                          (if (equal? reader-mode world:mode-auto)
                              (let* ([file-ext-pattern (pregexp "\\w+$")]
                                     [here-ext (string->symbol (car (regexp-match file-ext-pattern reader-here-path)))])
                                (cond
                                  [(equal? here-ext (world:get-pagetree-source-ext)) world:mode-pagetree]
                                  [(equal? here-ext (world:get-markup-source-ext)) world:mode-markup]
                                  [(equal? here-ext (world:get-markdown-source-ext)) world:mode-markdown]
                                  [else world:mode-preproc]))
                              reader-mode))
                        ;; change names of exports for local use
                        ;; so they don't conflict if this source is imported into another
                        (provide (except-out (all-defined-out) reader-here-path reader-mode parser-mode)
                                 (prefix-out inner: reader-here-path)
                                 (prefix-out inner: reader-mode)
                                 (prefix-out inner: parser-mode)) 
                        
                        ,(require+provide-directory-require-files path-string)
                        ,@file-contents)
                      (require 'pollen-lang-module)
                      (provide (all-from-out 'pollen-lang-module))
                      (module+ main
                        (require txexpr racket/string)
                        (if (or (equal? inner:parser-mode world:mode-preproc) (equal? inner:parser-mode world:mode-template))
                            (display ,(world:get-main-export))
                            (print (with-handlers ([exn:fail? (λ(exn) ((error '|pollen markup error| (string-join (cdr (string-split (exn-message exn) ": ")) ": "))))])
     (validate-txexpr ,(world:get-main-export)))))))
                   file-contents)))


(define-syntax-rule (define+provide-reader-in-mode mode)
  (begin
    (define reader-mode mode)
    (define custom-read-syntax (make-custom-read-syntax reader-mode))
    (define custom-read (make-custom-read custom-read-syntax))
    (define (get-info in mod line col pos)
      (λ (key default)
        (case key
          [(color-lexer)
           (define make-scribble-inside-lexer2
             (dynamic-require 'syntax-color/scribble-lexer 'make-scribble-inside-lexer (λ () #f)))
           (cond [make-scribble-inside-lexer2
                  (make-scribble-inside-lexer2 #:command-char #\◊)]
                 [else default])]
          [else default])))
    (provide (rename-out [custom-read read] [custom-read-syntax read-syntax]) get-info)))