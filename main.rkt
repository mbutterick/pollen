#lang racket/base
(require racket/list)
(require (planet mb/pollen/tools) (planet mb/pollen/main-helper))
(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [module-begin #%module-begin]))

(define-syntax-rule (module-begin expr ...)
  (#%module-begin
   
   ; this is here only so that dynamic-rerequire of a pollen module
   ; transitively reloads the extras also.
   ; if this isn't here, then dynamic-rerequire can't see them
   ; and thus they are not tracked for changes.
   (require-extras)
   
   ; We want our module language to support require & provide
   ; which are only supported at the module level, so ...
   ; create a submodule to contain the input
   ; and export as needed
   
   ; doclang2_raw is a clone of scribble/doclang2 with decode disabled
   ; helpful because it collects & exports content via 'doc
   (module pollen-inner (planet mb/pollen/lang/doclang2_raw)
     ; use same requires as top of main.rkt 
     ; (can't import them from surrounding module due to submodule rules)
     (require (planet mb/pollen/tools) (planet mb/pollen/main-helper))
     (require-extras #:provide #t) ; brings in the project require files
     
     ; #%top binding catches ids that aren't defined
     ; here, convert them to basic xexpr
     ; #%top is a syntax transformer that returns a function
     ; λ x captures all the args (vs. λ(x), which only catches one)
     ; and id is not spliced because it's syntax, not a true variable
     (define-syntax-rule (#%top . id)
       (λ x `(id ,@x)))
     
     expr ... ; body of module  
     
     ;; set up a hook for identifier 'here'
     ;; (but under a different name to avoid macrofication)
     (define inner-here here)  
     (provide (all-defined-out))) 
   
   (require 'pollen-inner) ; provides doc & #%top, among other things
   
   ;; prepare the elements, and append inner-here as meta.
   (define all-elements (append 
                                (cond
                                  ;; doc is probably a list, but might be a single string
                                  [(string? doc) (list doc)] 
                                  [(tagged-xexpr? doc) (list doc)] ; if it's a single nx, just leave it
                                  [(list? doc) doc]) ; if it's nx content, splice it in
                                (list `(meta "here" ,inner-here)))) ; append inner-here as meta
   
   ;; split out the metas now (in raw form)
   (define-values (main-raw metas-raw) 
     (extract-tag-from-xexpr 'meta (make-tagged-xexpr 'irrelevant-tag empty all-elements)))
   
   ;; Policy: here in the core lang, do as little to main as possible.
   ;; The point is just to set it up for further processing.
   ;; One of the annoyances of Scribble is its insistence on decoding.
   ;; Better just to pass through the minimally processed data.
   ;; Root is treated as a function. 
   ;; If it's not defined elsewhere, it just hits #%top and becomes a tagged-xexpr.
   (define main (apply root (tagged-xexpr-elements main-raw)))
   
   (define metas (make-meta-hash metas-raw))
   
   (provide main metas
            (except-out (all-from-out 'pollen-inner) inner-here) ; everything from user 
            (rename-out (inner-here here))) ; change identifier back (now safe from macrofication)
   
   (module+ main
     (displayln ";-------------------------")
     (displayln "; pollen decoded 'main")     
     (displayln ";-------------------------")
     main
     (displayln "")
     (displayln (format "(tagged-xexpr? main) ~a" (tagged-xexpr? main)))
     (displayln "")
     (displayln ";-------------------------")
     (displayln "; pollen 'metas")
     (displayln ";-------------------------")
     metas
     )))
