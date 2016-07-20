#lang racket/base
(require racket/syntax syntax/strip-context racket/class (for-syntax racket/base))
(require (only-in scribble/reader make-at-reader) "../setup.rkt" "project.rkt")
(provide (rename-out [reader-module-begin #%module-begin]) (all-from-out "../setup.rkt"))

(define (path-string->here-path path-string)
  (cond
    [(symbol? path-string) (symbol->string path-string)]
    [(equal? path-string "unsaved editor") path-string]
    [else (path->string path-string)]))


(define (infer-parser-mode reader-mode reader-here-path)
  (if (eq? reader-mode default-mode-auto)
      (let* ([file-ext-pattern (pregexp "\\w+$")]
             [here-ext (string->symbol (car (regexp-match file-ext-pattern reader-here-path)))]
             [auto-computed-mode (cond
                                   [(eq? here-ext (setup:pagetree-source-ext)) default-mode-pagetree]
                                   [(eq? here-ext (setup:markup-source-ext)) default-mode-markup]
                                   [(eq? here-ext (setup:markdown-source-ext)) default-mode-markdown]
                                   [else default-mode-preproc])])
        auto-computed-mode)
      reader-mode))

(define (custom-read p)
  (syntax->datum (custom-read-syntax (object-name p) p)))


(define (custom-read-syntax #:reader-mode [reader-mode #f] path-string p)
  (define source-stx (let ([read-inner (make-at-reader 
                                        #:command-char (setup:command-char)
                                        #:syntax? #t 
                                        #:inside? #t)])
                       (read-inner path-string p)))
  (define reader-here-path (path-string->here-path path-string))
  (define parser-mode-from-reader (infer-parser-mode reader-mode reader-here-path))
  (define parsed-syntax
    (strip-context
     (with-syntax* ([HERE-KEY (setup:here-path-key)]
                    [HERE-PATH reader-here-path]
                    [POLLEN-MOD-NAME 'pollen-module]
                    [DIRECTORY-REQUIRES (require+provide-directory-require-files path-string)]
                    [SOURCE-LINES source-stx]
                    [DOC (setup:main-export)]
                    [META-MOD (setup:meta-export)]
                    [PARSER-MODE-FROM-READER parser-mode-from-reader]
                    [POLLEN-MODULE-SYNTAX (let ([mod-stx #'(module POLLEN-MOD-NAME pollen
                                                             (define-meta HERE-KEY HERE-PATH) 
                                                             (provide (all-defined-out))
                                                             DIRECTORY-REQUIRES
                                                             . SOURCE-LINES)])
                                            (syntax-property mod-stx 'parser-mode-from-reader parser-mode-from-reader))])
       #'(module runtime-wrapper racket/base
           POLLEN-MODULE-SYNTAX
           (module META-MOD racket/base
             (require (submod ".." POLLEN-MOD-NAME META-MOD))
             (provide (all-from-out (submod ".." POLLEN-MOD-NAME META-MOD))))
           (require (submod pollen/private/runtime-config show) 'POLLEN-MOD-NAME)
           (provide (all-from-out 'POLLEN-MOD-NAME))
           (show DOC 'PARSER-MODE-FROM-READER HERE-PATH))))) ; HERE-PATH acts as "local" runtime config
  (syntax-property parsed-syntax
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
            (if my-make-scribble-inside-lexer
                (my-make-scribble-inside-lexer #:command-char my-command-char)
                default)]
           [(drracket:toolbar-buttons)
            (define my-make-drracket-buttons (dynamic-require 'pollen/private/drracket-buttons 'make-drracket-buttons))
            (my-make-drracket-buttons my-command-char)])]
        [(drracket:indentation)
         (dynamic-require 'scribble/private/indentation 'determine-spaces)]
        [else default]))))

(define-syntax-rule (reader-module-begin mode expr-to-ignore ...)
  (#%module-begin
   (define cgi custom-get-info) ; stash hygienic references to local funcs with macro-introduced identifiers
   (define cr custom-read) ; so they can be provided out
   (define (crs ps p) (custom-read-syntax #:reader-mode mode ps p))
   (provide (rename-out [cr read][crs read-syntax][cgi get-info]))))