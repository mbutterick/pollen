#lang racket/base
(require (only-in (planet mb/pollen/tools) as-list named-xexpr? decode tee ƒ)
         (only-in (planet mb/pollen/main-helper) split-metas require-extras))

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
     (require (planet mb/pollen/tools)
              (planet mb/pollen/template) ; for navigation functions
              (planet mb/pollen/main-helper)) ; for split-metas and get-here
     (require-and-provide-extras) ; brings in the project require files
     
     ; #%top binding catches ids that aren't defined
     ; here, convert them to basic xexpr
     ; #%top is a syntax transformer that returns a function
     ; ƒ x captures all the args (vs. ƒ(x), which only catches one)
     ; and id is not spliced because it's syntax, not a true variable
     (define-syntax-rule (#%top . id)
       (ƒ x `(id ,@x)))
     
     expr ... ; body of module   
     (define inner-here here) ; set up a hook for 'here (different name to avoid macrofication)
     (provide (all-defined-out))
     (provide (all-from-out ; pollen file should bring its requires
               (planet mb/pollen/tools)
               (planet mb/pollen/template)))) 
   
   (require 'pollen-inner) ; provides 'doc
   
   (define text (as-list doc)) ; if single line, text will be a string
   (set! text (if (named-xexpr? text) ; different setup depending on whether we have
                  `(main ,text) ; a whole xexpr or
                  `(main ,@text))) ; just xexpr content
   
   ; take out the metas so they don't goof up decoding
   (define-values (raw-main metas) (split-metas text))
   
   ; splice in any included files
   ; todo: is this a safe operation?
   ; assume that main will never have an attr field
   ; because attr would parse out as content.
   (set! raw-main (splice-xexpr-content raw-main))
   
   ; decode
   (define main (decode raw-main))
   
   ; append metas to decoded 
   (when metas
     (set! main (append main metas)))
   
   (provide main text ; module language add-ons
            (except-out (all-from-out 'pollen-inner) inner-here) ; everything from user 
            (rename-out (inner-here here))) ; change back to 'here
   
   (module+ main 
     ((tee (ƒ(x)x) (ƒ(x)(format "named-xexpr? ~a" (named-xexpr? main)))) main)))) 
