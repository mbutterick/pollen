#lang racket/base
(require (for-syntax racket/base syntax/strip-context racket/syntax pollen/world) pollen/decode pollen/pagetree racket/list pollen/world markdown)

(provide (all-defined-out) (all-from-out pollen/world))

(define-syntax-rule (define+provide-module-begin-in-mode PARSER-MODE-ARG)
  (begin
    (provide (except-out (all-from-out racket/base) #%module-begin)
             (rename-out [pollen-module-begin #%module-begin]))
    (define-syntax (pollen-module-begin stx)
      (syntax-case stx ()
        [(_ EXPR (... ...))
         (with-syntax ([DOC (format-id #'(EXPR (... ...)) "~a" (world:current-main-export))])
           (replace-context #'(EXPR (... ...))
                            #'(#%module-begin
                               (module inner pollen/doclang-raw ; exports result as doc-raw
                                 (require pollen/top pollen/world)
                                 (provide #%top (all-defined-out) (all-from-out pollen/world))
                                 EXPR (... ...))
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
                                        [doc-elements (dropf doc-raw (λ(ln) (equal? ln "\n")))])
                                   (apply proc doc-elements)))
                               (provide DOC (except-out (all-from-out 'inner) doc-raw #%top)))))])))) ; hide internal exports