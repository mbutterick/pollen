#lang racket/base
(require (for-syntax racket/base racket/syntax pollen/world) pollen/world)

(provide (all-defined-out) (all-from-out pollen/world))

(define-syntax-rule (define+provide-module-begin-in-mode MODE-ARG)
  (begin
    (provide (except-out (all-from-out racket/base) #%module-begin)
             (rename-out [pollen-module-begin #%module-begin]))
    (define-syntax (pollen-module-begin stx)
      (syntax-case stx ()
        [(_ EXPR (... ...))
         (with-syntax ([DOC (datum->syntax stx (world:current-main-export))]
                       [METAS (datum->syntax stx (world:current-meta-export))]
                       [META (datum->syntax stx (world:current-meta-tag-name))]
                       [INNER (datum->syntax stx ''inner)])
           #'(#%module-begin
              (module inner pollen/doclang-raw
                ;; doclang_raw is a version of scribble/doclang with the decoder disabled
                ;; first three lines are positional arguments for doclang-raw
                doc-raw ; id of export
                (λ(x) x) ; post-process function
                () ; prepended exprs
                
                ;; Change behavior of undefined identifiers with #%top
                ;; Get project values from world
                (require pollen/top pollen/world)
                (provide (all-defined-out) (all-from-out pollen/top pollen/world))
                EXPR (... ...))
              
              (require INNER 'inner pollen/metas) ; import inner twice: INNER for external use, 'inner for internal
              
              ;; in an inline module, reader-here-path and parser-mode are undefined 
              ;; (because there's no reader)
              ;; but they'll become tag functions courtesy of #%top
              ;; so that's how we can detect if they are undefined
              (define here-path (if (procedure? inner:reader-here-path)
                                    "anonymous-module"
                                    inner:reader-here-path))
              (define parser-mode (if (procedure? inner:parser-mode)
                                      MODE-ARG
                                      inner:parser-mode))
              
              ;; set up the DOC export
              (define doc-elements (if (list? doc-raw) ; discard all newlines at front of file
                                       ((dynamic-require 'racket/list 'dropf) doc-raw (λ(i) (equal? i "\n"))) 
                                       doc-raw)) ; single line
              (define doc-with-metas (list* 'placeholder-root `(META (here-path ,here-path)) doc-elements))
              (define-values (doc-without-metas METAS) (split-metas-to-hash doc-with-metas))
              (define DOC
                (let ([proc (cond
                              [(equal? parser-mode world:mode-pagetree)
                               (λ xs ((dynamic-require 'pollen/pagetree 'decode-pagetree) xs))]
                              ;; 'root is the hook for the decoder function.
                              ;; If it's not a defined identifier, it just hits #%top and becomes `(root ,@body ...)
                              [(equal? parser-mode world:mode-markup) root]
                              [(equal? parser-mode world:mode-markdown)
                               (λ xs (apply root (apply (compose1 (dynamic-require 'markdown 'parse-markdown) string-append)
                                                        (map (dynamic-require 'pollen/decode 'to-string) xs))))]
                              ;; for preprocessor output, just make a string.
                              [else (λ xs (apply string-append (map (dynamic-require 'pollen/decode 'to-string) xs)))])]
                      [doc-elements-without-metas (cdr doc-without-metas)])
                  (apply proc doc-elements-without-metas)))
              
              ;; hide the exports that were only for internal use.
              (provide DOC METAS (except-out (all-from-out INNER) doc-raw #%top))))]))))
