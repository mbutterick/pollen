#lang racket/base
(require (for-syntax racket/base syntax/strip-context racket/syntax "../setup.rkt" "split-metas.rkt")
         "to-string.rkt" "../pagetree.rkt" "splice.rkt" "../setup.rkt")
(require "../setup.rkt")
(provide (except-out (all-from-out racket/base) #%module-begin)
         (all-from-out "../setup.rkt")
         (rename-out [dialect-module-begin #%module-begin]))


(define-syntax-rule (dialect-module-begin PARSER-MODE-FROM-EXPANDER . READER-SUBMOD-AND-OTHER-EXPRS)
  (#%module-begin
   (require racket/base)
   (provide (except-out (all-from-out racket/base) #%module-begin)
            (rename-out [pollen-module-begin #%module-begin]))
   (define-syntax (pollen-module-begin stx)
     (syntax-case stx ()
       [(_ . EXPRS)
        (let-values ([(meta-hash exprs-without-metas) (split-metas (syntax->datum #'EXPRS) (setup:define-meta-name))])
          (with-syntax (;; 'parser-mode-from-reader will be #f for an inline submodule
                        [PARSER-MODE-FROM-READER-PROPERTY (syntax-property stx 'parser-mode-from-reader)]
                        [META-HASH meta-hash]
                        [EXPRS-WITHOUT-METAS exprs-without-metas]
                        [METAS (setup:meta-export)]
                        [META-MOD (setup:meta-export)]
                        [ROOT (setup:main-root-node)]
                        [NEWLINE (setup:newline)]
                        [MODE-PAGETREE default-mode-pagetree]
                        [MODE-MARKUP default-mode-markup]
                        [MODE-MARKDOWN default-mode-markdown]
                        [SPLICING-TAG (setup:splicing-tag)]
                        [DOC (setup:main-export)]
                        [DOC-RAW (generate-temporary 'pollen-)]); prevents conflicts with other imported Pollen sources
            (replace-context
             #'here
             #'(#%module-begin
                (module META-MOD racket/base
                  (provide METAS)
                  (define METAS META-HASH))
                
                (module inner pollen/private/doclang-raw
                  DOC-RAW ; positional arg for doclang-raw that sets name of export.
                  (require pollen/top (submod ".." META-MOD) pollen/core pollen/setup)
                  (provide (all-defined-out) #%top (all-from-out (submod ".." META-MOD) pollen/core))
                  . EXPRS-WITHOUT-METAS)
                
                (require 'inner)
                
                (define DOC
                  (let* ([parser-mode (or 'PARSER-MODE-FROM-READER-PROPERTY PARSER-MODE-FROM-EXPANDER)]
                         [proc (case parser-mode
                                 [(MODE-PAGETREE) decode-pagetree]
                                 [(MODE-MARKUP) (λ (xs) (apply ROOT xs))] ; if `root` undefined, it becomes a default tag function
                                 [(MODE-MARKDOWN) (λ (xs) (apply ROOT (map strip-empty-attrs ((dynamic-require 'markdown 'parse-markdown) (apply string-append (map to-string xs))))))]
                                 [else (λ (xs) (apply string-append (map to-string xs)))])] ; string output for preprocessor
                         ;; drop leading newlines, as they're often the result of `defines` and `requires`
                         [doc-elements (or (memf (λ (ln) (not (equal? ln NEWLINE))) DOC-RAW) null)]
                         [doc-elements-spliced (splice doc-elements 'SPLICING-TAG)])
                    (proc doc-elements-spliced)))
                
                (provide DOC METAS (except-out (all-from-out 'inner) DOC-RAW #%top))))))]))
   . READER-SUBMOD-AND-OTHER-EXPRS))