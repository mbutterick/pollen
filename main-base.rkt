#lang racket/base
(require (for-syntax racket/base syntax/strip-context racket/syntax pollen/world racket/list) pollen/decode pollen/pagetree racket/list pollen/world markdown)

(provide (all-defined-out) (all-from-out pollen/world))

(define-for-syntax (split-metas tree)
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

(define-syntax-rule (define+provide-module-begin-in-mode PARSER-MODE-ARG)
  (begin
    (provide (except-out (all-from-out racket/base) #%module-begin)
             (rename-out [pollen-module-begin #%module-begin]))
    (define-syntax (pollen-module-begin stx)
      (syntax-case stx ()
        [(_ EXPR (... ...))
         (let-values ([(meta-keys meta-values expr-without-metas) (split-metas (syntax->datum #'(EXPR (... ...))))])
           (with-syntax ([(EXPR-WITHOUT-METAS (... ...)) (datum->syntax #'(EXPR (... ...)) expr-without-metas)]
                         [(KEY (... ...)) (datum->syntax #'(EXPR (... ...)) meta-keys)]
                         [(VALUE (... ...)) (datum->syntax #'(EXPR (... ...)) meta-values)]
                         [METAS (format-id #'(EXPR (... ...)) "~a" (world:current-meta-export))]
                         [META-MOD (format-symbol "~a" (world:current-meta-export))]
                         [DOC (format-id #'(EXPR (... ...)) "~a" (world:current-main-export))]
                         [DOC-RAW (generate-temporary)]); prevents conflicts with other imported Pollen sources
             (replace-context #'(EXPR (... ...))
                              #'(#%module-begin
                                 (module META-MOD racket/base
                                   (provide (all-defined-out))
                                   (define METAS (apply hash (append (list 'KEY VALUE) (... ...)))))
                                 
                                 (module inner pollen/doclang-raw
                                   DOC-RAW ; positional arg for doclang-raw that sets name of export.
                                   (require pollen/top pollen/world)
                                   (require (submod ".." META-MOD))
                                   (provide (all-defined-out) #%top (all-from-out pollen/world (submod ".." META-MOD)))
                                   EXPR-WITHOUT-METAS (... ...))
                                 
                                 (require 'inner)
                                 (define DOC
                                   (let* ([parser-mode-undefined? (procedure? inner:parser-mode)] ; if undefined, #%top makes it a procedure
                                          [parser-mode (if parser-mode-undefined? PARSER-MODE-ARG inner:parser-mode)]
                                          [proc (cond
                                                  [(eq? parser-mode world:mode-pagetree) (λ xs (decode-pagetree xs))]
                                                  [(eq? parser-mode world:mode-markup) root] ; if `root` undefined, it becomes a default tag function
                                                  [(eq? parser-mode world:mode-markdown)
                                                   (λ xs (apply root (apply (compose1 parse-markdown string-append) (map to-string xs))))]
                                                  [else ; for preprocessor output, just make a string
                                                   (λ xs (apply string-append (map to-string xs)))])]
                                          ;; drop leading newlines, as they're often the result of `defines` and `requires`
                                          [doc-elements (dropf DOC-RAW (λ(ln) (equal? ln "\n")))])
                                     (apply proc doc-elements)))
                                 (provide DOC METAS (except-out (all-from-out 'inner) DOC-RAW #%top))))))])))) ; hide internal exports