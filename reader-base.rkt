#lang racket/base
(require racket/syntax syntax/strip-context)
(require (only-in scribble/reader make-at-reader) pollen/world pollen/project racket/list)
(provide define+provide-reader-in-mode (all-from-out pollen/world))


(define (make-custom-read custom-read-syntax-proc) 
  (λ(p) (syntax->datum (custom-read-syntax-proc (object-name p) p))))


(define (split-metas tree)
  (define (meta-matcher x) ; meta has form (define-meta key value)
    (and (list? x) (>= (length x) 3) (eq? (first x) (world:current-define-meta-name))))
  (define matches empty)
  (define rest
    (let loop ([x tree])
      (cond
        [(meta-matcher x)
         (set! matches (cons x matches))
         (loop empty)]
        [(list? x)
         (define-values (new-matches rest) (partition meta-matcher x))
         (set! matches (append new-matches matches))
         (map loop rest)]
        [else x])))
  (let ([meta-key second][meta-value third])
    (values (map meta-key matches) (map meta-value matches) rest)))


(define (make-custom-read-syntax reader-mode)
  (λ (path-string p)
    (define read-inner (make-at-reader 
                        #:command-char (if (or (eq? reader-mode world:mode-template) 
                                               (and (string? path-string)
                                                    (regexp-match (pregexp (format "\\.~a$" (world:current-template-source-ext))) path-string)))
                                           (world:current-template-command-char)
                                           (world:current-command-char))
                        #:syntax? #t 
                        #:inside? #t))
    (define source-stx (read-inner path-string p))
    (define-values (meta-keys meta-values meta-free-file-data) (split-metas (syntax->datum source-stx)))
    (define reader-here-path (cond
                               [(symbol? path-string) (symbol->string path-string)]
                               [(equal? path-string "unsaved editor") path-string]
                               [else (path->string path-string)]))
    (define parser-mode (if (eq? reader-mode world:mode-auto)
                            (let* ([file-ext-pattern (pregexp "\\w+$")]
                                   [here-ext (string->symbol (car (regexp-match file-ext-pattern reader-here-path)))]
                                   [auto-computed-mode (cond
                                                         [(eq? here-ext (world:current-pagetree-source-ext)) world:mode-pagetree]
                                                         [(eq? here-ext (world:current-markup-source-ext)) world:mode-markup]
                                                         [(eq? here-ext (world:current-markdown-source-ext)) world:mode-markdown]
                                                         [else world:mode-preproc])])
                              auto-computed-mode)
                            reader-mode))
    (define meta-keys-plus-here (cons (world:current-here-path-key) meta-keys)) ; here-path at front so it can be overridden
    (define meta-values-plus-here (cons reader-here-path meta-values))
    (with-syntax ([(KEY ...) (datum->syntax source-stx meta-keys-plus-here)]
                  [(VALUE ...) (datum->syntax source-stx meta-values-plus-here)])
      (syntax-property
       (replace-context source-stx
                        #`(module runtime-wrapper racket/base
                            (module metas racket/base
                              (provide (all-defined-out))
                              (define #,(world:current-meta-export) (apply hash (append (list 'KEY VALUE) ...))))
                            
                            (module pollen-lang-module pollen
                              (define parser-mode '#,parser-mode) ; change names of exports for local use, to avoid conflicts
                              (provide (except-out (all-defined-out) parser-mode)
                                       (prefix-out inner: parser-mode)) 
                              #,(require+provide-directory-require-files path-string)
                              (require (submod ".." ".." metas)) ; get metas from adjacent submodule
                              (provide (all-from-out (submod ".." ".." metas)))
                              #,@meta-free-file-data)
                            
                            (require (submod pollen/runtime-config show) 'pollen-lang-module)
                            (provide (all-from-out 'pollen-lang-module))
                            (show #,(world:current-main-export) inner:parser-mode)))
       'module-language
       '#(pollen/language-info get-language-info #f)))))


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