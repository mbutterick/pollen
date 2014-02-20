#lang racket/base

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [new-module-begin #%module-begin]))

(define-syntax-rule (new-module-begin body-exprs ...)
  (#%module-begin
   (module inner pollen/lang/doclang_raw
     ;; first three lines are positional arguments for doclang_raw
     main-raw ; id of export
     (λ(x) x) ; post-process function
     () ; prepended exprs
     
     (require pollen/lang/lang-helper)
     (require-project-require-files)
     (provide (all-defined-out))
     
     ;; Change behavior of undefined identifiers
     (require pollen/top)
     (provide (all-from-out pollen/top))
     
     ;; Build 'inner-here-path and 'inner-here
     (define inner-here-path (get-here-path))
     (require (only-in pollen/world PROJECT_ROOT))
     (require (only-in racket/path find-relative-path))
     (require (only-in pollen/file-tools ->output-path))
     (define inner-here (path->string (->output-path (find-relative-path PROJECT_ROOT inner-here-path))))
     
     body-exprs ...)
   
   (require 'inner)
   
   ;; Split out the metas. We want to catch user-defined metas too.
   ;; First, append "here-path" and "here" as metas.
   ;; Because they might have been overridden by custom metas in the source.
   (require txexpr)   
   (define one-with-everything `(placeholder-root 
                                 (meta "here-path" ,inner-here-path)
                                 (meta "here" ,inner-here)                         
                                 ,@(cdr main-raw))) ;; cdr strips initial linebreak
   
   (define is-meta-element? (λ(x) (and (txexpr? x) (equal? 'meta (get-tag x)))))
   (define-values (main-without-metas meta-elements) 
     (splitf-txexpr one-with-everything is-meta-element?))
   (define meta-element->assoc (λ(x) (cons (cadr x) (caddr x))))
   (define metas (make-hash (map meta-element->assoc meta-elements)))
   
   
   ;; set up the 'main export
   (define here-ext (car (regexp-match #px"\\w+$" inner-here-path)))
   (define wants-decoder? (member here-ext (list "pd" "ptree")))
   (define main (apply (if wants-decoder?
                           ;; 'root is the hook for the decoder function.
                           ;; If it's not a defined identifier, it just hits #%top and becomes `(root ,@body ...)
                           root
                           ;; for textual (preprocessor-style) output. Converts x-expressions to HTML.
                           (λ xs (apply string-append (map (dynamic-require 'xml 'xexpr->string) xs))))
                       (cdr main-without-metas))) ;; cdr strips placeholder-root tag
   
   
   ;; derive 'here & 'here-path from the hash (because they might have been overridden in the source) 
   (define here (hash-ref metas "here"))
   (define here-path (hash-ref metas "here-path"))
   
   (provide metas main here here-path
            ;; hide the exports that were only for internal use
            (except-out (all-from-out 'inner) inner-here inner-here-path main-raw #%top))
   
   ;; for output in DrRacket
   (module+ main
     (if wants-decoder? 
         (print main) 
         (display main)))))
