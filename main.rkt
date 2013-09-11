#lang racket/base
(require racket/list)
(require (planet mb/pollen/tools) (planet mb/pollen/main-helper))
(require (only-in (planet mb/pollen/pmap-decode) pmap-source-decode))
(require (only-in (planet mb/pollen/predicates) pmap?))
(require (only-in (planet mb/pollen/pollen-file-tools) has-ext?))
(require (only-in (planet mb/pollen/world) POLLEN_MAP_EXT))
(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [module-begin #%module-begin]))

(define-syntax-rule (module-begin expr ...)
  (#%module-begin
   
   ;; this is here only so that dynamic-rerequire of a pollen module
   ;; transitively reloads the extras also.
   ;; if this isn't here, then dynamic-rerequire can't see them
   ;; and thus they are not tracked for changes.
   (require-extras)
   
   ;; We want our module language to support require & provide
   ;; which are only supported at the module level, so ...
   ;; create a submodule to contain the input
   ;; and export as needed
   
   ;; doclang2_raw is a clone of scribble/doclang2 with decode disabled
   ;; helpful because it collects & exports content via 'doc
   (module pollen-inner (planet mb/pollen/lang/doclang2_raw)
     ;; use same requires as top of main.rkt 
     ;; (can't import them from surrounding module due to submodule rules)
     (require (planet mb/pollen/tools) (planet mb/pollen/main-helper))
     (require-and-provide-extras) ; brings in the project require files
     
     ;; #%top binding catches ids that aren't defined
     ;; here, convert them to basic xexpr
     ;; #%top is a syntax transformer that returns a function
     ;; λ x captures all the args (vs. λ(x), which only catches one)
     ;; and id is not spliced because it's syntax, not a true variable
     ;; WARNING! This is convenient for writing pollen documents
     ;; (which is why it works this way)
     ;; but it makes debugging tricky, because an undefined (symbol item ...)
     ;; is just treated as a valid tagged-xexpr, not an undefined function.
     (define-syntax-rule (#%top . id)
       (λ x `(id ,@x)))
     
     expr ... ; body of module  
     
     ;; set up a hook for identifier 'here'
     ;; (but under a different name to avoid macrofication)
     (define inner-here here)  
     (provide (all-defined-out))
     (provide (all-from-out ; pollen file should bring its requires
               (planet mb/pollen/tools)))) 
   
   (require 'pollen-inner) ; provides doc & #%top, among other things
   
   ;; prepare the elements, and append inner-here as meta.
   (define all-elements (cons 
                         ;; append inner-here as meta
                         ;; put it first so it can be overridden by custom meta later on
                         `(meta "here" ,inner-here)
                         (cond
                           ;; doc is probably a list, but might be a single string
                           [(string? doc) (list doc)] 
                           [(tagged-xexpr? doc) (list doc)] ; if it's a single nx, just leave it
                           [(list? doc) doc]))) ; if it's nx content, splice it in
   
   
   ;; split out the metas now (in raw form)
   (define-values (metas-raw main-raw) 
     (split-tag-from-xexpr 'meta (make-tagged-xexpr 'irrelevant-tag empty all-elements)))
   
   (define metas (make-meta-hash metas-raw))
   
   ;; Policy: here in the core lang, do as little to main as possible.
   ;; The point is just to set it up for further processing.
   ;; Unlike Scribble, which insists on decoding,
   ;; Pollen just passes through the minimally processed data.
   ;; one exception: if file extension marks it as pmap, send it to the pmap decoder instead.
   
   ;; this tests inner-here (which is always the file name)
   ;; rather than (get metas 'here) which might have been overridden.
   ;; Because if it's overridden to something other than *.pmap, 
   ;; pmap processing will fail.
   ;; This defeats rule that pmap file suffix triggers pmap decoding.
   (define here-is-pmap? (pmap-source? (->path inner-here)))
   
   (define main (apply (if here-is-pmap?
                           ;; pmap source files will go this way,
                           pmap-source-decode
                           ;; ... but other files, including pollen, will go this way.
                           ;; Root is treated as a function. 
                           ;; If it's not defined elsewhere, 
                           ;; it just hits #%top and becomes a tagged-xexpr.
                           root) (tagged-xexpr-elements main-raw)))
   
   (provide main metas
            (except-out (all-from-out 'pollen-inner) inner-here) ; everything from user 
            (rename-out (inner-here here))) ; change identifier back (now safe from macrofication)
   
   (module+ main
     (displayln ";-------------------------")
     (displayln (string-append "; pollen decoded 'main" (if here-is-pmap? " (as pmap)" "")))     
     (displayln ";-------------------------")
     main
     (displayln "")
     
     (if here-is-pmap?
         (displayln (format "(pmap? main) ~a" (pmap? main)))
         (displayln (format "(tagged-xexpr? main) ~a" (tagged-xexpr? main))))
     (displayln "")
     (displayln ";-------------------------")
     (displayln "; pollen 'metas")
     (displayln ";-------------------------")
     metas
     )))
