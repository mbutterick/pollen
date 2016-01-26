#lang racket/base
(require racket/syntax syntax/strip-context racket/class)
(require (only-in scribble/reader make-at-reader) pollen/world "project.rkt" racket/list)
(provide define+provide-reader-in-mode (all-from-out pollen/world))


(define (make-custom-read custom-read-syntax-proc) 
  (λ(p) (syntax->datum (custom-read-syntax-proc (object-name p) p))))


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
    (define post-parser-syntax
      (with-syntax ([HERE-KEY (format-id source-stx "~a" (world:current-here-path-key))]
                    [HERE-PATH (datum->syntax source-stx reader-here-path)]
                    [POLLEN-MOD (format-symbol "~a" (gensym))] ; prevents conflicts with other imported Pollen sources
                    [PARSER-MODE-VALUE (format-symbol "~a" parser-mode)]
                    [DIRECTORY-REQUIRES (datum->syntax source-stx (require+provide-directory-require-files path-string))]
                    [(SOURCE-LINE ...) source-stx]
                    [DOC (format-id source-stx "~a" (world:current-main-export))])
        (replace-context
         source-stx
         #'(module runtime-wrapper racket/base
             (module POLLEN-MOD pollen
               (define-meta HERE-KEY HERE-PATH) 
               (define parser-mode 'PARSER-MODE-VALUE)
               (provide (except-out (all-defined-out) parser-mode)
                        (prefix-out inner: parser-mode)) ; avoids conflicts with importing modules
               DIRECTORY-REQUIRES
               SOURCE-LINE ...)
             (require (submod pollen/private/runtime-config show) 'POLLEN-MOD)
             (provide (all-from-out 'POLLEN-MOD))
             (show DOC inner:parser-mode HERE-PATH))))) ; HERE-PATH acts as "local" runtime config
    (syntax-property post-parser-syntax
                     'module-language
                     `#(pollen/private/language-info get-language-info ,reader-here-path)))) ; reader-here-path acts as "top" runtime config

(define-syntax-rule (define+provide-reader-in-mode mode)
  (begin
    (define reader-mode mode)
    (define custom-read-syntax (make-custom-read-syntax reader-mode))
    (define custom-read (make-custom-read custom-read-syntax))
    (define (get-info in mod line col pos)
      ;; DrRacket caches source file information per session,
      ;; so we can do the same to avoid multiple searches for the command char.
      (let ([command-char-cache (make-hash)])
        (λ (key default)
          (case key
            [(color-lexer drracket:toolbar-buttons) ; only do source-path searching if we have one of these keys
             (define maybe-source-path (with-handlers ([exn:fail? (λ(exn) #f)])
                                         ;; Robert Findler does not endorse `get-filename` here,
                                         ;; because it's sneaky and may not always work.
                                         ;; OTOH Scribble relies on it, so IMO it's highly unlikely to change.
                                         (let ([maybe-definitions-frame (object-name in)])
                                           (send maybe-definitions-frame get-filename)))) ; will be #f if unsaved file
             (define my-command-char (hash-ref! command-char-cache maybe-source-path (λ _ (world:current-command-char maybe-source-path))))
             (case key
               [(color-lexer)
                (define my-make-scribble-inside-lexer
                  (dynamic-require 'syntax-color/scribble-lexer 'make-scribble-inside-lexer (λ () #f)))
                (cond [my-make-scribble-inside-lexer
                       (my-make-scribble-inside-lexer #:command-char my-command-char)]
                      [else default])]
               [(drracket:toolbar-buttons)
                (define my-make-drracket-buttons (dynamic-require 'pollen/private/drracket-buttons 'make-drracket-buttons))
                (my-make-drracket-buttons my-command-char)])]
            [else default]))))
    (provide (rename-out [custom-read read] [custom-read-syntax read-syntax]) get-info)))