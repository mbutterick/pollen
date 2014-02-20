#lang racket/base

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [new-module-begin #%module-begin]))

(define-syntax-rule (new-module-begin body-exprs ...)
  (#%module-begin
   (module inner pollen/lang/doclang_raw
     ;; first three lines are positional arguments for doclang_raw
     ;; doclang_raw is a version of scribble/doclang with the decoder disabled
     main-raw ; id of export
     (λ(x) x) ; post-process function
     () ; prepended exprs
     
     (require pollen/lang/lang-helper)
     (require-and-provide-project-require-files)
     
     ;; Change behavior of undefined identifiers with #%top
     (require pollen/top)
     (provide (all-from-out pollen/top))
     
     ;; Build 'inner-here-path and 'inner-here
     (define inner-here-path (get-here-path))
     (require (only-in racket/path find-relative-path))
     (define inner-here (path->string (path-replace-suffix (find-relative-path (current-directory) inner-here-path) "")))
     
     (provide (all-defined-out))
     
     body-exprs ...)
   
   (require 'inner)
   
   ;; Split out the metas. 
   (require txexpr)   
   (define main-txexpr `(placeholder-root ,@(cdr main-raw))) ;; cdr strips initial linebreak
   (define is-meta-element? (λ(x) (and (txexpr? x) (equal? 'meta (car x)))))
   (define-values (main-without-metas meta-elements) 
     (splitf-txexpr main-txexpr is-meta-element?))
   (define meta-element->assoc (λ(x) (cons (cadr x) (caddr x))))
   ;; Prepend 'here-path and 'here as metas so they can be overridden by metas embedded in source.
   (define metas (make-hash (map meta-element->assoc (cons `(meta "here-path" ,inner-here-path) 
                                                           (cons `(meta "here" ,inner-here) meta-elements)))))
   
   
   
   
   ;; set up the 'main export
   (require pollen/decode)
   (require (only-in racket/list filter-not))
   (define here-ext (car (regexp-match #px"\\w+$" inner-here-path)))
   (define wants-decoder? (member here-ext (list "pd" "ptree")))
   ;(print (cdr main-without-metas))
   (define main (apply (cond
                         [(equal? here-ext "ptree") (λ xs (decode (cons 'ptree-root xs)
                                                      #:xexpr-elements-proc (λ(xs) (filter-not whitespace? xs))))]
                         ;; 'root is the hook for the decoder function.
                         ;; If it's not a defined identifier, it just hits #%top and becomes `(root ,@body ...)
                         [wants-decoder? root]
                         ;; for preprocessor output, just make a string. Converts x-expressions to HTML.
                         [else (λ xs (apply string-append (map (dynamic-require 'xml 'xexpr->string) xs)))])
(cdr main-without-metas))) ;; cdr strips placeholder-root tag


;; derive 'here & 'here-path from the hash (because they might have been overridden in the source) 
(define here (hash-ref metas "here"))
(define here-path (hash-ref metas "here-path"))

(provide metas main here here-path
         ;; hide the exports that were only for internal use.
         (except-out (all-from-out 'inner) inner-here inner-here-path main-raw #%top))

;; for output in DrRacket
(module+ main
  (if wants-decoder? 
      (print main) 
      (display main)))))
