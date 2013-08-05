#lang racket/base
(require (planet mb/pollen/tools)
         (planet mb/pollen/main-helper))

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
   (module pollen-inner (planet mb/pollen/doclang2_raw)
     ; use same requires as top of main.rkt 
     ; (can't import them from surrounding module due to submodule rules)
     (require (planet mb/pollen/tools)
              (planet mb/pollen/main-helper))
     (require-extras #:provide #t) ; brings in the project require files
     
     ; #%top binding catches ids that aren't defined
     ; here, convert them to basic xexpr
     ; #%top is a syntax transformer that returns a function
     ; λ x captures all the args (vs. λ(x), which only catches one)
     ; and id is not spliced because it's syntax, not a true variable
     (define-syntax-rule (#%top . id)
       (λ x `(id ,@x)))
     
     expr ... ; body of module   
     (define inner-here here) ; set up a hook for identifier 'here' (different name to avoid macrofication)
     (provide (all-defined-out))) 
   
   (require 'pollen-inner) ; provides 'doc
   
   (define text (merge-newlines (as-list doc))) ; if single line, text will be a string
   (define main (append
                 ; different setup depending on whether we have
                 (if (named-xexpr? text) 
                     `(main ,text) ; a whole xexpr or
                     `(main ,@text)) ; just xexpr content 
                 (list (meta "here" inner-here)))) ; append inner-here as meta
   
   (provide main)
   
   (module+ main
     (print main)
     (displayln "")
     (displayln (format "named-xexpr? ~a" (named-xexpr? main)))))) 
