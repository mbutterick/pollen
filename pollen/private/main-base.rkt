#lang racket/base
(require (for-syntax racket/base syntax/strip-context racket/syntax "../setup.rkt" "split-metas.rkt")
         "to-string.rkt" "../pagetree.rkt" "splice.rkt" "../setup.rkt")
(require "../setup.rkt")
(provide (except-out (all-from-out racket/base) #%module-begin)
         (all-from-out "../setup.rkt")
         (rename-out [dialect-module-begin #%module-begin]))


(define-syntax-rule (dialect-module-begin <parser-mode-in> <outer-expr> ...)
  (#%module-begin
   (require racket/base)
   (provide (except-out (all-from-out racket/base) #%module-begin)
            (rename-out [pollen-module-begin #%module-begin]))
   (define-syntax (pollen-module-begin stx)
     (syntax-case stx ()
       [(_ <expr> (... ...))
        (let-values ([(meta-hash expr-without-metas) (split-metas (syntax->datum #'(<expr> (... ...))) (setup:define-meta-name))])
          (with-syntax ([META-HASH meta-hash]
                        [(EXPR-WITHOUT-METAS (... ...)) expr-without-metas]
                        [METAS (format-id #f "~a" (setup:meta-export))]
                        [META-MOD (format-symbol "~a" (setup:meta-export))]
                        [ROOT (format-id #f "~a" (setup:main-root-node))]
                        [NEWLINE (datum->syntax #f (setup:newline))]
                        [MODE-PAGETREE default-mode-pagetree]
                        [MODE-MARKUP default-mode-markup]
                        [MODE-MARKDOWN default-mode-markdown]
                        [SPLICING-TAG (setup:splicing-tag)]
                        [DOC (format-id #f "~a" (setup:main-export))]
                        [DOC-RAW (generate-temporary 'pollen-)]); prevents conflicts with other imported Pollen sources
            (replace-context #'(<expr> (... ...))
                             #'(#%module-begin
                                (module META-MOD racket/base
                                  (provide METAS)
                                  (define METAS META-HASH))
                                
                                (module inner pollen/private/doclang-raw
                                  DOC-RAW ; positional arg for doclang-raw that sets name of export.
                                  (require pollen/top pollen/setup pollen/core)
                                  (require (submod ".." META-MOD))
                                  (provide (all-defined-out) #%top (all-from-out (submod ".." META-MOD) pollen/core))
                                  EXPR-WITHOUT-METAS (... ...))
                                
                                (require 'inner)
                                
                                (define DOC
                                  (let* ([parser-mode-undefined? (procedure? inner:parser-mode)] ; if undefined, #%top makes it a procedure
                                         [parser-mode (if parser-mode-undefined? <parser-mode-in> inner:parser-mode)]
                                         [proc (case parser-mode
                                                 [(MODE-PAGETREE) decode-pagetree]
                                                 [(MODE-MARKUP) (λ(xs) (apply ROOT xs))] ; if `root` undefined, it becomes a default tag function
                                                 [(MODE-MARKDOWN)
                                                  (λ(xs) (apply ROOT (map strip-empty-attrs ((dynamic-require 'markdown 'parse-markdown) (apply string-append (map to-string xs))))))]
                                                 [else (λ(xs) (apply string-append (map to-string xs)))])] ; string output for preprocessor
                                         ;; drop leading newlines, as they're often the result of `defines` and `requires`
                                         [doc-elements (or (memf (λ(ln) (not (equal? ln NEWLINE))) DOC-RAW) null)]
                                         [doc-elements-spliced (splice doc-elements 'SPLICING-TAG)])
                                    (proc doc-elements-spliced)))
                                
                                (provide DOC METAS (except-out (all-from-out 'inner) DOC-RAW #%top))))))]))
   <outer-expr> ...))