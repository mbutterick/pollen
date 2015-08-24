#lang racket/base
(require (for-syntax racket/base racket/syntax pollen/world) pollen/decode pollen/pagetree racket/list pollen/world markdown)

(provide (all-defined-out) (all-from-out pollen/world))

(define-syntax-rule (define+provide-module-begin-in-mode MODE-ARG)
  (begin
    (provide (except-out (all-from-out racket/base) #%module-begin)
             (rename-out [pollen-module-begin #%module-begin]))
    (define-syntax (pollen-module-begin stx)
      (syntax-case stx ()
        [(_ expr (... ...))
         (with-syntax ([EXPRS #'(expr (... ...))])
           (datum->syntax #'EXPRS
                          `(#%module-begin
                            (module inner pollen/doclang-raw ; exports result as doc-raw
                              (require pollen/top pollen/world)
                              (provide #%top (all-defined-out) (all-from-out pollen/world))
                              ,@(syntax->datum #'EXPRS))
                            (require 'inner)
                            (define ,(world:current-main-export)
                              (let* ([parser-mode-undefined? (procedure? inner:parser-mode)] ; if undefined, #%top makes it a procedure
                                     [parser-mode (if parser-mode-undefined?
                                                      MODE-ARG
                                                      inner:parser-mode)]
                                     [proc (cond
                                             [(eq? parser-mode world:mode-pagetree) (λ xs (decode-pagetree xs))]
                                             [(eq? parser-mode world:mode-markup) root] ; if `root` undefined, it becomes a default tag function
                                             [(eq? parser-mode world:mode-markdown)
                                              (λ xs (apply root (apply (compose1 parse-markdown string-append)
                                                                       (map to-string xs))))]
                                             [else ; for preprocessor output, just make a string
                                              (λ xs (apply string-append (map to-string xs)))])]
                                     [doc-elements (if (list? doc-raw) ; discard all newlines at front of multi-line file
                                                       (dropf doc-raw (λ(ln) (equal? ln "\n"))) 
                                                       doc-raw)]) ; single line
                                (apply proc doc-elements)))
                            (provide ,(world:current-main-export) (except-out (all-from-out 'inner) doc-raw #%top))) #'EXPRS))])))) ; hide internal exports