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
          (with-syntax ([<meta-hash> meta-hash]
                        [(<expr-without-metas> (... ...)) expr-without-metas]
                        [<metas> (format-id #f "~a" (setup:meta-export))]
                        [<meta-mod> (format-symbol "~a" (setup:meta-export))]
                        [<root> (format-id #f "~a" (setup:main-root-node))]
                        [<newline> (datum->syntax #f (setup:newline))]
                        [<mode-pagetree> default-mode-pagetree]
                        [<mode-markup> default-mode-markup]
                        [<mode-markdown> default-mode-markdown]
                        [<splicing_tag> (setup:splicing-tag)]
                        [<doc> (format-id #f "~a" (setup:main-export))]
                        [<doc-raw> (generate-temporary 'pollen-)]); prevents conflicts with other imported Pollen sources
            (replace-context #'(<expr> (... ...))
                             #'(#%module-begin
                                (module <meta-mod> racket/base
                                  (provide <metas>)
                                  (define <metas> <meta-hash>))
                                
                                (module inner pollen/private/doclang-raw
                                  <doc-raw> ; positional arg for doclang-raw that sets name of export.
                                  (require pollen/top pollen/setup pollen/core)
                                  (require (submod ".." <meta-mod>))
                                  (provide (all-defined-out) #%top (all-from-out (submod ".." <meta-mod>) pollen/core))
                                  <expr-without-metas> (... ...))
                                
                                (require 'inner)
                                
                                (define <doc>
                                  (let* ([parser-mode-undefined? (procedure? inner:parser-mode)] ; if undefined, #%top makes it a procedure
                                         [parser-mode (if parser-mode-undefined? <parser-mode-in> inner:parser-mode)]
                                         [proc (case parser-mode
                                                 [(<mode-pagetree>) decode-pagetree]
                                                 [(<mode-markup>) (λ(xs) (apply <root> xs))] ; if `root` undefined, it becomes a default tag function
                                                 [(<mode-markdown>)
                                                  (λ(xs) (apply <root> (map strip-empty-attrs ((dynamic-require 'markdown 'parse-markdown) (apply string-append (map to-string xs))))))]
                                                 [else (λ(xs) (apply string-append (map to-string xs)))])] ; string output for preprocessor
                                         ;; drop leading newlines, as they're often the result of `defines` and `requires`
                                         [doc-elements (or (memf (λ(ln) (not (equal? ln <newline>))) <doc-raw>) null)]
                                         [doc-elements-spliced (splice doc-elements '<splicing_tag>)])
                                    (proc doc-elements-spliced)))
                                
                                (provide <doc> <metas> (except-out (all-from-out 'inner) <doc-raw> #%top))))))]))
   <outer-expr> ...))