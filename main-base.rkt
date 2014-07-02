#lang racket/base
(require (for-syntax racket/base racket/syntax) pollen/world)

(provide (all-defined-out) (all-from-out pollen/world))

(define-syntax (define+provide-module-begin-in-mode stx)
  (syntax-case stx ()
    [(_ mode-arg)
     (with-syntax ([new-module-begin (format-id stx "new-module-begin")])
       #'(begin
           (provide (except-out (all-from-out racket/base) #%module-begin)
                   (rename-out [new-module-begin #%module-begin]))
           (define-syntax (new-module-begin stx-arg)
             (syntax-case stx-arg ()
               [(_ body-exprs (... ...))
                (syntax-protect 
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
                      (provide (all-from-out pollen/top pollen/world))
                      
                      (provide (all-defined-out))
                      
                      body-exprs (... ...))
                    
                    (require 'inner)
                    
                    
                    ;; if reader-here-path is undefined, it will become a proc courtesy of #%top
                    ;; therefore that's how we can detect if it's undefined
                    (define here-path (if (procedure? inner:reader-here-path) "anonymous-module" inner:reader-here-path))
                    
                    
                    ;; set the parser mode based on reader mode
                    ;; todo: this won't work with inline submodules
                    (define parser-mode 
                      (if (not (procedure? inner:reader-mode))
                         (if (equal? inner:reader-mode world:mode-auto)
                            (let* ([file-ext-pattern (pregexp "\\w+$")]
                                   [here-ext (string->symbol (car (regexp-match file-ext-pattern here-path)))])
                              (cond
                                [(equal? here-ext world:pagetree-source-ext) world:mode-pagetree]
                                [(equal? here-ext world:markup-source-ext) world:mode-markup]
                                [(equal? here-ext world:markdown-source-ext) world:mode-markdown]
                                [else world:mode-preproc]))
                            inner:reader-mode)
                         mode-arg))
                    
                    
                    ;; Split out the metas.   
                    (define (split-metas-to-hash x)
                      (define (meta-key x) (car (caadr x)))
                      (define (meta-value x) (cadr (caadr x)))
                      (define is-meta-element? (λ(x) (and (list? x) ; possible txexpr
                                                         (= (length x) 2) ; = tag + attribute
                                                         (equal? 'meta (car x)) ; tag is 'meta
                                                         (symbol? (meta-key x)) ; attribute key is symbol
                                                         (string? (meta-value x))))) ; attribute value is string
                      (define (splitter x)
                        (define meta-acc null)
                        (define (split-metas x)
                          (cond
                            [(list? x) (define-values (new-metas rest) (values (filter is-meta-element? x) (filter (compose1 not is-meta-element?) x)))
                                      (set! meta-acc (append new-metas meta-acc))
                                      (map split-metas rest)]
                            [else x]))
                        (define result (split-metas x))
                        (values result meta-acc))
                      
                      (define-values (doc-without-metas meta-elements) (splitter x))
                      (define meta-element->assoc (λ(x) (let ([key (meta-key x)][value (meta-value x)]) (cons key value))))
                      (define metas (make-hash (map meta-element->assoc meta-elements)))
                      (values doc-without-metas metas)) 
                    
                    (require racket/list)
                    
                    (define doc-with-metas
                        `(placeholder-root 
                          ,@(cons (meta 'here-path: here-path) 
                                 (if (list? doc-raw) 
                                    (dropf doc-raw (λ(i) (equal? i "\n"))) ; discard all newlines at front of file
                                    doc-raw)))) 
                    
                    (define-values (doc-without-metas metas) (split-metas-to-hash doc-with-metas))
                    
                    ;; set up the 'doc export
                    (require pollen/decode)
                    (define doc (apply (cond
                                         [(equal? parser-mode world:mode-pagetree) (λ xs ((dynamic-require 'pollen/pagetree 'decode-pagetree) xs))]
                                         ;; 'root is the hook for the decoder function.
                                         ;; If it's not a defined identifier, it just hits #%top and becomes `(root ,@body ...)
                                         [(equal? parser-mode world:mode-markup) root]
                                         [(equal? parser-mode world:mode-markdown) (λ xs (apply root (apply (compose1 (dynamic-require 'markdown 'parse-markdown) string-append) xs)))]
                                         ;; for preprocessor output, just make a string.
                                         [else (λ xs (apply string-append (map to-string xs)))]) ; default mode is preprocish
                                      (cdr doc-without-metas))) ;; cdr strips placeholder-root tag
                    
                    
                    (provide metas doc
                            ;; hide the exports that were only for internal use.
                            (except-out (all-from-out 'inner) doc-raw #%top))
                    
                    ;; for output in DrRacket
                    (module+ main
                      (if (or (equal? parser-mode world:mode-preproc) (equal? parser-mode world:mode-template))
                         (display doc)
                         (print doc)))))]))))]))