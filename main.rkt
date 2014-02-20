#lang racket/base

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [new-module-begin #%module-begin]))

(define-syntax-rule (new-module-begin body-exprs ...)
  (#%module-begin
   ;; first three lines are positional arguments
   (module inner pollen/lang/doclang_raw
     main-raw
     (λ(x) (cdr x)) ;; chop first linebreak with cdr
     ()
     (require pollen/main-helper pollen/top )
     (require-project-require-files)
     (provide (all-defined-out))
     
     ;; Build 'here
     (define here-path (get-here-path))
     (require (only-in xml xexpr->string))
     (require (only-in racket/path find-relative-path))
     (require (only-in pollen/file-tools ->output-path))
     (require (only-in pollen/world PROJECT_ROOT))
     (define (path->pnode path)
       (path->string (->output-path (find-relative-path PROJECT_ROOT path))))
     (define here (path->pnode here-path))
     
     body-exprs ...)
   
   (require 'inner)
   
   ;; function to split tag out of txexpr
   (require txexpr)
   
   ;; split out the metas. Might include user-defined metas.
   ;; But first, append here-path and here as meta.
   ;;  so they can be overridden by custom meta later
   ;; 'root is the hook for the decoder function.
   ;; If it's not defined elsewhere, it just hits #%top and becomes a txexpr.
   
   (define one-with-everything `(root 
                                 ,@(cons `(meta "here-path" ,here-path)
                                         (cons `(meta "here" ,here)                         
                                               main-raw))))
   
   (define is-meta-element? (λ(x) (and (txexpr? x) (equal? 'meta (get-tag x)))))
   (define-values (metas-raw main-without-metas) 
     (splitf-txexpr one-with-everything is-meta-element?))
   
   (define meta-element-to-pair (λ(x) (cons (cadr x) (caddr x))))
   (define metas (make-hash (map meta-element-to-pair metas-raw)))
   (define main main-without-metas)
   
   (provide (all-from-out 'inner) metas main)
   
   (module+ main
     (print main))))
