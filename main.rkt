#lang racket/base
(require "main-imports.rkt")
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
   (module pollen-inner pollen/lang/doclang2_raw
     ;; use same requires as top of main.rkt 
     ;; (can't import them from surrounding module due to submodule rules)
     ;; todo: how to keep these two lists in sync?
     ;; and why doesn't this work:
     ;;     (require pollen/main-imports)
     ;;     (provide (all-from-out pollen/main-imports))
     (require pollen/tools pollen/main-helper pollen/top pollen/ptree sugar txexpr)
     (require-and-provide-extras) ; brings in the project require files
     
     expr ... ; body of module  
     
     ;; set up a hook for identifier 'here'
     ;; (but under a different name to avoid macrofication)
     (define inner-here-path here-path)  
     (provide (all-defined-out))
     (provide (all-from-out ; pollen file should bring its requires
               pollen/tools))) 
   
   (require 'pollen-inner) ; provides doc & #%top, among other things
   
   (define here ((bound/c path->pnode) inner-here-path))
   
   ;; prepare the elements, and append inner-here-path as meta.
   ;; put it first so it can be overridden by custom meta later on
   (define all-elements (cons `(meta "here-path" ,inner-here-path)
                              (cons `(meta "here" ,here)                         
                                    (cond
                                      ;; doc is probably a list, but might be a single string
                                      [(string? doc) (list doc)] 
                                      ;; if it's a single nx, just leave it
                                      [(txexpr? doc) (list doc)]
                                      ;; if it's nx content, splice it in
                                      [(list? doc) doc])))) 
   
   
   ;; split out the metas now (in raw form)
   (define-values (metas-raw main-raw) 
     ((bound/c split-tag-from-xexpr) 'meta (make-txexpr 'irrelevant-tag empty all-elements)))
   
   (define metas (make-meta-hash metas-raw))
   
   ;; Policy: here in the core lang, do as little to main as possible.
   ;; The point is just to set it up for further processing.
   ;; Unlike Scribble, which insists on decoding,
   ;; Pollen just passes through the minimally processed data.
   ;; one exception: if file extension marks it as ptree, send it to the ptree decoder instead.
   
   ;; this tests inner-here (which is always the file name)
   ;; rather than (get metas 'here) which might have been overridden.
   ;; Because if it's overridden to something other than *.ptree, 
   ;; ptree processing will fail.
   ;; This defeats rule that ptree file suffix triggers ptree decoding.
   (define here-is-ptree? ((bound/c ptree-source?) ((bound/c ->path) inner-here-path)))
   
   (define main (apply (if here-is-ptree?
                           ;; ptree source files will go this way,
                           (bound/c ptree-source-decode)
                           ;; ... but other files, including pollen, will go this way.
                           ;; Root is treated as a function. 
                           ;; If it's not defined elsewhere, 
                           ;; it just hits #%top and becomes a txexpr.
                           root) ((bound/c get-elements) main-raw)))
   
   
   (provide main metas here
            (except-out (all-from-out 'pollen-inner) inner-here-path) ; everything from user 
            (rename-out (inner-here-path here-path))) ; change identifier back (now safe from macrofication)
   
   (module+ main
     (displayln ";-------------------------")
     (displayln (string-append "; pollen decoded 'main" (if here-is-ptree? " (as ptree)" "")))     
     (displayln ";-------------------------")
     main
     (displayln "")
     
     (if here-is-ptree?
         (displayln (format "(ptree? main) ~a" (ptree? main)))
         (displayln (format "(txexpr? main) ~a" (txexpr? main))))
     (displayln "")
     (displayln ";-------------------------")
     (displayln "; pollen 'metas")
     (displayln ";-------------------------")
     metas
     )))
