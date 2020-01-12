#lang racket/base
(require racket/syntax
         syntax/strip-context
         racket/class
         racket/string
         racket/runtime-path
         racket/match
         setup/getinfo
         sugar/file
         (for-syntax racket/base)
         (only-in scribble/reader make-at-reader)
         "../setup.rkt"
         "project.rkt")
(provide (rename-out [reader-module-begin #%module-begin]) (all-from-out "../setup.rkt"))

(define (source-name->pollen-require-path source-name)
  ;; the `path-string` passed in from `read-syntax` can actually be `any/c`
  ;; captures paths, strings, "unsaved editor", path-strings, symbols
  ((if (syntax? source-name) syntax-source values) source-name))

(define (infer-parser-mode reader-mode reader-here-path)
  (match reader-mode
    [(== default-mode-auto)
     (match (cond [(get-ext reader-here-path) => string->symbol])
       [(== (setup:pagetree-source-ext)) default-mode-pagetree]
       [(== (setup:markup-source-ext)) default-mode-markup]
       [(== (setup:markdown-source-ext)) default-mode-markdown]
       [_ default-mode-preproc])]
    [_ reader-mode]))

(define (custom-read p) (syntax->datum (custom-read-syntax (object-name p) p)))

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
  (define command-char-cache (make-hash))
  (λ (key default)
    (case key
      ;; only do source-path searching if we have one of these two keys
      [(color-lexer drracket:toolbar-buttons) 
       (define maybe-source-path
         (with-handlers ([exn:fail? (λ (exn) #false)])
           ;; Robert Findler does not endorse `get-filename` here,
           ;; because it's sneaky and may not always work.
           ;; OTOH Scribble relies on it, so IMO it's highly unlikely to change.
           (send (object-name in) get-filename)))
       (define my-command-char
         (hash-ref! command-char-cache maybe-source-path (λ () (setup:command-char maybe-source-path))))
       (case key
         [(color-lexer)
          (match (dynamic-require 'syntax-color/scribble-lexer 'make-scribble-inside-lexer (λ () #false))
            [(? procedure? make-lexer) (make-lexer #:command-char my-command-char)]
            [_ default])]
         [(drracket:toolbar-buttons)
          (match (dynamic-require 'pollen/private/drracket-buttons 'make-drracket-buttons (λ () #false))
            [(? procedure? make-buttons) (make-buttons my-command-char)])])]
      [(drracket:indentation)
       (λ (text pos)
         (define line-idx (send text position-line pos))
         (define line-start-pos (send text line-start-position line-idx))
         (define line-end-pos (send text line-end-position line-idx))
         (define first-vis-pos
           (or
            (for/first ([pos (in-range line-start-pos line-end-pos)]
                        #:unless (char-blank? (send text get-character pos)))
                       pos)
            line-start-pos))
         (- first-vis-pos line-start-pos))]
      [(drracket:default-filters)
       ;; derive this from `module-suffixes` entry in main info.rkt file
       (define module-suffixes ((get-info/full info-dir) 'module-suffixes))
       (define filter-strings (for/list ([suffix (in-list module-suffixes)])
                                        (format "*.~a" suffix)))
       (list (list "Pollen sources" (string-join filter-strings ";")))]
      [(drracket:default-extension)
       (symbol->string
        (match mode
          [(== default-mode-auto) (setup:preproc-source-ext)]
          [(== default-mode-preproc) (setup:preproc-source-ext)]
          [(== default-mode-markdown) (setup:markdown-source-ext)]
          [(== default-mode-markup) (setup:markup-source-ext)]
          [(== default-mode-pagetree) (setup:pagetree-source-ext)]))]
      [else default])))

(define-syntax-rule (reader-module-begin mode . _)
  (#%module-begin
   (define cgi (custom-get-info mode)) ; stash hygienic references to local funcs with macro-introduced identifiers
   (define cr custom-read) ; so they can be provided out
   ;; allow six-argument arity to be compatible with `debug`
   (define (crs ps p . _) (custom-read-syntax #:reader-mode mode ps p))
   (provide (rename-out [cr read][crs read-syntax][cgi get-info]))))