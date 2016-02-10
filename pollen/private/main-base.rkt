#lang racket/base
(require (for-syntax racket/base syntax/strip-context racket/syntax "../setup.rkt" "split-metas.rkt")
         "to-string.rkt" "../pagetree.rkt" "splice.rkt" "../setup.rkt" ) ; need world here to resolve PARSER-MODE-ARG
(provide (all-defined-out))

(define-syntax-rule (define+provide-module-begin-in-mode PARSER-MODE-ARG)
  (begin
    (provide (except-out (all-from-out racket/base) #%module-begin)
             (rename-out [pollen-module-begin #%module-begin]))
    (define-syntax (pollen-module-begin stx)
      (syntax-case stx ()
        [(_ EXPR (... ...))
         (let-values ([(meta-hash expr-without-metas) (split-metas (syntax->datum #'(EXPR (... ...))) (setup:define-meta-name))])
           (with-syntax ([META-HASH (datum->syntax #f meta-hash)]
                         [(EXPR-WITHOUT-METAS (... ...)) (datum->syntax #f expr-without-metas)]
                         [METAS (format-id #f "~a" (setup:meta-export))]
                         [META-MOD (format-symbol "~a" (setup:meta-export))]
                         [ROOT (format-id #f "~a" (setup:main-root-node))]
                         [NEWLINE (datum->syntax #f (setup:newline))]
                         [MODE-PAGETREE (datum->syntax #f default-mode-pagetree)]
                         [MODE-MARKUP (datum->syntax #f default-mode-markup)]
                         [MODE-MARKDOWN (datum->syntax #f default-mode-markdown)]
                         [SPLICING_TAG (datum->syntax #f (setup:splicing-tag))]
                         [DOC (format-id #f "~a" (setup:main-export))]
                         [DOC-RAW (generate-temporary 'pollen-)]); prevents conflicts with other imported Pollen sources
             (replace-context #'(EXPR (... ...))
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
                                          [parser-mode (if parser-mode-undefined? PARSER-MODE-ARG inner:parser-mode)]
                                          [proc (cond
                                                  [(eq? parser-mode 'MODE-PAGETREE) decode-pagetree]
                                                  [(eq? parser-mode 'MODE-MARKUP) (λ(xs) (apply ROOT xs))] ; if `root` undefined, it becomes a default tag function
                                                  [(eq? parser-mode 'MODE-MARKDOWN)
                                                   (λ(xs) (apply ROOT (map strip-empty-attrs ((dynamic-require 'markdown 'parse-markdown) (apply string-append (map to-string xs))))))]
                                                  [else (λ(xs) (apply string-append (map to-string xs)))])] ; string output for preprocessor
                                          ;; drop leading newlines, as they're often the result of `defines` and `requires`
                                          [doc-elements (or (memf (λ(ln) (not (equal? ln NEWLINE))) DOC-RAW) null)]
                                          [doc-elements-spliced (splice doc-elements 'SPLICING_TAG)])
                                     (proc doc-elements-spliced)))
                                 
                                 (provide DOC METAS (except-out (all-from-out 'inner) DOC-RAW #%top))))))])))) ; hide internal exports