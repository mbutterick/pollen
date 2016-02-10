#lang racket/base
(require racket/syntax syntax/strip-context racket/class (for-syntax racket/base racket/syntax))
(require (only-in scribble/reader make-at-reader) "../setup.rkt" "project.rkt" racket/function)
(provide (rename-out [reader-module-begin #%module-begin]) (all-from-out "../setup.rkt"))

(define current-reader-mode (make-parameter #f))


(define (custom-read p)
  (syntax->datum (custom-read-syntax (object-name p) p)))


(define (custom-read-syntax path-string p)
  (define read-inner (make-at-reader 
                      #:command-char (setup:command-char)
                      #:syntax? #t 
                      #:inside? #t))
  (define source-stx (read-inner path-string p))
  (define reader-here-path (cond
                             [(symbol? path-string) (symbol->string path-string)]
                             [(equal? path-string "unsaved editor") path-string]
                             [else (path->string path-string)]))
  (define parser-mode (if (eq? (current-reader-mode) default-mode-auto)
                          (let* ([file-ext-pattern (pregexp "\\w+$")]
                                 [here-ext (string->symbol (car (regexp-match file-ext-pattern reader-here-path)))]
                                 [auto-computed-mode (cond
                                                       [(eq? here-ext (setup:pagetree-source-ext)) default-mode-pagetree]
                                                       [(eq? here-ext (setup:markup-source-ext)) default-mode-markup]
                                                       [(eq? here-ext (setup:markdown-source-ext)) default-mode-markdown]
                                                       [else default-mode-preproc])])
                            auto-computed-mode)
                          (current-reader-mode)))
  (define post-parser-syntax
    (with-syntax ([HERE-KEY (format-id #f "~a" (setup:here-path-key))]
                  [HERE-PATH (datum->syntax #f reader-here-path)]
                  [POLLEN-MOD (format-symbol "~a" (gensym))] ; prevents conflicts with other imported Pollen sources
                  [PARSER-MODE-VALUE (format-symbol "~a" parser-mode)]
                  [DIRECTORY-REQUIRES (datum->syntax #f (require+provide-directory-require-files path-string))]
                  [(SOURCE-LINE ...) source-stx]
                  [DOC (format-id #f "~a" (setup:main-export))])
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
                   `#(pollen/private/language-info get-language-info ,reader-here-path))) ; reader-here-path acts as "top" runtime config

(define (custom-get-info in mod line col pos)
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
         (define my-command-char (hash-ref! command-char-cache maybe-source-path (λ _ (setup:command-char maybe-source-path))))
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

(define-syntax (reader-module-begin stx)
  (syntax-case stx ()
    [(_ mode expr-to-ignore ...)
     (with-syntax ([cr (generate-temporary)]
                   [crs (generate-temporary)]
                   [cgi (generate-temporary)])
     #'(#%module-begin
         (current-reader-mode mode)
         (define cgi custom-get-info)
         (define cr custom-read)
         (define crs custom-read-syntax)
         (provide (rename-out [cr read][crs read-syntax][cgi get-info]))))]))