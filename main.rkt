#lang racket/base

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [new-module-begin #%module-begin]))

(define-syntax-rule (new-module-begin body-exprs ...)
  (#%module-begin
   (module inner pollen/lang/doclang-raw
     ;; doclang_raw is a version of scribble/doclang with the decoder disabled
     ;; first three lines are positional arguments for doclang-raw
     doc-raw ; id of export
     (λ(x) x) ; post-process function
     () ; prepended exprs
     
     (require pollen/lang/inner-lang-helper)
     (require-and-provide-project-require-files) ; only works if current-directory is set correctly
     
     ;; Change behavior of undefined identifiers with #%top
     (require pollen/top)
     (provide (all-from-out pollen/top))
     
     ;; Get project values
     (require pollen/world)
     (provide (all-from-out pollen/world))
     
     ;; Build 'inner-here-path and 'inner-here
     (define (here-path->here here-path)
       (path->string (path-replace-suffix (pollen-find-relative-path (world:current-project-root) here-path) "")))
     (define inner-here-path (get-here-path))
     (define inner-here (here-path->here inner-here-path))
     
     (provide (all-defined-out))
     
     body-exprs ...)
   
   (require 'inner)
   
   ;; Split out the metas.   
   (require txexpr)   
   (define (split-metas-to-hash tx)
     ;; return tx without metas, and meta hash
     (define is-meta-element? (λ(x) (and (txexpr? x) (equal? 'meta (car x)))))
     (define-values (doc-without-metas meta-elements) 
       (splitf-txexpr tx is-meta-element?))
     (define meta-element->assoc (λ(x) (let ([key (car (caadr x))][value (cadr (caadr x))]) (cons key value))))
     (define metas (make-hash (map meta-element->assoc meta-elements)))
     (values doc-without-metas metas))
   
   (define doc-txexpr `(placeholder-root 
                         ,@(cons (meta 'here: inner-here) 
                                 (cons (meta 'here-path: inner-here-path) 
                                       ;; cdr strips initial linebreak, but make sure doc-raw isn't blank
                                       (if (and (list? doc-raw) (> 0 (length doc-raw))) (cdr doc-raw) doc-raw))))) 
   
   (define-values (doc-without-metas metas) (split-metas-to-hash doc-txexpr))
   
   
   ;; set up the 'doc export
   (require pollen/decode)

   ;; set the parser mode based on reader mode
   (define parser-mode 
     (if (reader-mode . equal? . world:reader-mode-auto)
         (let* ([file-ext-pattern (pregexp "\\w+$")]
                [here-ext (car (regexp-match file-ext-pattern inner-here-path))])
           (cond
             [(equal? (string->symbol here-ext) world:ptree-source-ext) world:reader-mode-ptree]
             [(equal? (string->symbol here-ext) world:markup-source-ext) world:reader-mode-markup]
             [else world:reader-mode-preproc]))
         reader-mode))
      
   (define doc (apply (cond
                         [(equal? parser-mode world:reader-mode-ptree) 
                          (λ xs (decode (cons world:ptree-root-node xs)
                                        #:xexpr-elements-proc (λ(xs) (filter (compose1 not (def/c whitespace?)) xs))))]
                         ;; 'root is the hook for the decoder function.
                         ;; If it's not a defined identifier, it just hits #%top and becomes `(root ,@body ...)
                         [(equal? parser-mode world:reader-mode-markup) root]
                         ;; for preprocessor output, just make a string.
                         [else (λ xs (apply string-append (map to-string xs)))])
                       (cdr doc-without-metas))) ;; cdr strips placeholder-root tag
   
   
   (provide metas doc
            ;; hide the exports that were only for internal use.
            (except-out (all-from-out 'inner) inner-here inner-here-path doc-raw #%top))
   
   ;; for output in DrRacket
   (module+ main
     (if (equal? parser-mode world:reader-mode-preproc)
         (display doc)
         (print doc)))))
