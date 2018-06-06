#lang racket/base
(require racket/syntax
         syntax/strip-context
         racket/class
         racket/string
         racket/runtime-path
         setup/getinfo
         (for-syntax racket/base)
         (only-in scribble/reader make-at-reader)
         "../setup.rkt"
         "project.rkt")
(provide (rename-out [reader-module-begin #%module-begin]) (all-from-out "../setup.rkt"))

(define (source-name->pollen-require-path source-name)
  ;; the `path-string` passed in from `read-syntax` can actually be `any/c`
  (if (syntax? source-name)
      (syntax-source source-name)
      ;; captures paths, strings, "unsaved editor", path-strings, symbols
      source-name))

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


(define (custom-read-syntax #:reader-mode [reader-mode #f] source-name input-port)
  (define source-stx (let ([read-inner (make-at-reader 
                                        #:command-char (setup:command-char)
                                        #:syntax? #t 
                                        #:inside? #t)])
                       (read-inner source-name input-port)))
  (define pollen-require-path (source-name->pollen-require-path source-name))
  (define reader-here-path (format "~a" pollen-require-path))
  (define parser-mode-from-reader (infer-parser-mode reader-mode reader-here-path))

  (strip-context
   (with-syntax* ([POLLEN-MOD-NAME 'pollen-module]
                  ;; the next two exist only in the reader because they are specific to file-based Pollen sources.
                  ;; an inline Pollen submodule doesn't have "pollen.rkt" or `here-path` 
                  [POLLEN-REQUIRE-AND-PROVIDES (require+provide-directory-require-files pollen-require-path)]
                  [HERE-PATH reader-here-path]
                  [HERE-KEY (setup:here-path-key)]
                  [SOURCE-LINES source-stx]
                  [DOC (setup:main-export)]
                  [META-MOD (setup:meta-export)]
                  [METAS-ID (setup:meta-export)]
                  [PARSER-MODE-FROM-READER parser-mode-from-reader])
     #'(module runtime-wrapper racket/base
         (module configure-runtime racket/base
           (require pollen/private/runtime-config)
           (configure HERE-PATH)) ; HERE-PATH acts as "top" runtime config when module is main
         (module POLLEN-MOD-NAME pollen/private/main-base
           'PARSER-MODE-FROM-READER
           (define-meta HERE-KEY HERE-PATH)
           POLLEN-REQUIRE-AND-PROVIDES
           . SOURCE-LINES)
         (module META-MOD racket/base
           (require (submod ".." POLLEN-MOD-NAME META-MOD))
           (provide METAS-ID))
         (require (only-in pollen/private/runtime-config show) 'POLLEN-MOD-NAME)
         (provide (all-from-out 'POLLEN-MOD-NAME))
         (show DOC 'PARSER-MODE-FROM-READER HERE-PATH))))) ; HERE-PATH otherwise acts as "local" runtime config 

(define-runtime-path info-dir "..")

(define ((custom-get-info mode) in mod line col pos)
  ;; DrRacket caches source file information per session,
  ;; so we can do the same to avoid multiple searches for the command char.
  (let ([command-char-cache (make-hash)])
    (λ (key default)
      (case key
        [(color-lexer drracket:toolbar-buttons) ; only do source-path searching if we have one of these keys
         (define maybe-source-path (with-handlers ([exn:fail? (λ (exn) #f)])
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
        [(drracket:default-filters)
         ;; derive this from `module-suffixes` entry in main info.rkt file
         (define module-suffixes ((get-info/full info-dir) 'module-suffixes))
         (define filter-strings (for/list ([suffix (in-list module-suffixes)])
                                          (format "*.~a" suffix)))
         (list (list "Pollen sources" (string-join filter-strings ";")))]
        [(drracket:default-extension)
         (symbol->string
          (cond
            [(eq? mode default-mode-auto) (setup:preproc-source-ext)]
            [(eq? mode default-mode-preproc) (setup:preproc-source-ext)]
            [(eq? mode default-mode-markdown) (setup:markdown-source-ext)]
            [(eq? mode default-mode-markup) (setup:markup-source-ext)]
            [(eq? mode default-mode-pagetree) (setup:pagetree-source-ext)]))]
        [else default]))))

(define-syntax-rule (reader-module-begin mode . _)
  (#%module-begin
   (define cgi (custom-get-info mode)) ; stash hygienic references to local funcs with macro-introduced identifiers
   (define cr custom-read) ; so they can be provided out
   (define (crs ps p) (custom-read-syntax #:reader-mode mode ps p))
   (provide (rename-out [cr read][crs read-syntax][cgi get-info]))))