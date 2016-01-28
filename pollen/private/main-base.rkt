#lang racket/base
(require (for-syntax racket/base syntax/strip-context racket/syntax "../world.rkt" "split-metas.rkt")
         "to-string.rkt" "../pagetree.rkt" "splice.rkt" "../world.rkt") ; need world here to resolve PARSER-MODE-ARG
(provide (all-defined-out))

(define-syntax-rule (define+provide-module-begin-in-mode PARSER-MODE-ARG)
  (begin
    (provide (except-out (all-from-out racket/base) #%module-begin)
             (rename-out [pollen-module-begin #%module-begin]))
    (define-syntax (pollen-module-begin stx)
      (syntax-case stx ()
        [(_ EXPR (... ...))
         (let-values ([(meta-hash expr-without-metas) (split-metas (syntax->datum #'(EXPR (... ...))) (world:current-define-meta-name))])
           (with-syntax ([META-HASH (datum->syntax #'(EXPR (... ...)) meta-hash)]
                         [(EXPR-WITHOUT-METAS (... ...)) (datum->syntax #'(EXPR (... ...)) expr-without-metas)]
                         [METAS (format-id #'(EXPR (... ...)) "~a" (world:current-meta-export))]
                         [META-MOD (format-symbol "~a" (world:current-meta-export))]
                         [ROOT (format-id #'(EXPR (... ...)) "~a" (world:current-main-root-node))]
                         [NEWLINE (datum->syntax #'(EXPR (... ...)) (world:current-newline))]
                         [MODE-PAGETREE (datum->syntax #'(EXPR (... ...)) world:mode-pagetree)]
                         [MODE-MARKUP (datum->syntax #'(EXPR (... ...)) world:mode-markup)]
                         [MODE-MARKDOWN (datum->syntax #'(EXPR (... ...)) world:mode-markdown)]
                         [SPLICING_TAG (datum->syntax #'(EXPR (... ...)) (world:current-splicing-tag))]
                         [DOC (format-id #'(EXPR (... ...)) "~a" (world:current-main-export))]
                         [DOC-RAW (generate-temporary 'pollen-)]); prevents conflicts with other imported Pollen sources
             (replace-context #'(EXPR (... ...))
                              #'(#%module-begin
                                 (module META-MOD racket/base
                                   (provide METAS)
                                   (define METAS META-HASH))
                                 
                                 (module inner pollen/private/doclang-raw
                                   DOC-RAW ; positional arg for doclang-raw that sets name of export.
                                   (require pollen/top pollen/world)
                                   (require (submod ".." META-MOD))
                                   (provide (all-defined-out) #%top (all-from-out (submod ".." META-MOD)))
                                   EXPR-WITHOUT-METAS (... ...))
                                 
                                 (require 'inner)
                                 
                                 (define DOC
                                   (let* ([parser-mode-undefined? (procedure? inner:parser-mode)] ; if undefined, #%top makes it a procedure
                                          [parser-mode (if parser-mode-undefined? PARSER-MODE-ARG inner:parser-mode)]
                                          [proc (cond
                                                  [(eq? parser-mode 'MODE-PAGETREE) decode-pagetree]
                                                  [(eq? parser-mode 'MODE-MARKUP) (λ(xs) (apply ROOT xs))] ; if `root` undefined, it becomes a default tag function
                                                  [(eq? parser-mode 'MODE-MARKDOWN)
                                                   (λ(xs) (apply ROOT ((dynamic-require 'markdown 'parse-markdown) (apply string-append (map to-string xs)))))]
                                                  [else (λ(xs) (apply string-append (map to-string xs)))])] ; string output for preprocessor
                                          ;; drop leading newlines, as they're often the result of `defines` and `requires`
                                          [doc-elements (or (memf (λ(ln) (not (equal? ln NEWLINE))) DOC-RAW) null)]
                                          [doc-elements-spliced (splice doc-elements 'SPLICING_TAG)])
                                     (proc doc-elements-spliced)))
                                 
                                 (provide DOC METAS (except-out (all-from-out 'inner) DOC-RAW #%top))))))])))) ; hide internal exports