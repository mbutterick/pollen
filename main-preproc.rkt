#lang racket/base

(require "main-preproc-imports.rkt" sugar)
(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [module-begin #%module-begin]))

(require (only-in scribble/text output)
         (only-in racket/list flatten))

(define-syntax-rule (module-begin expr ...)
  (#%module-begin
   
   ; We want our module language to support require & provide
   ; which are only supported at the module level, so ...
   ; create a submodule to contain the input
   ; and export as needed
   
   ; doclang2_raw is a clone of scribble/doclang2 with decode disabled
   ; helpful because it collects & exports content via 'doc
   (module pollen-inner pollen/lang/doclang2_raw
     (require pollen/tools pollen/main-helper pollen/top)
     (require-and-provide-extras) ; brings in the project require files
     
     expr ...) ; body of module 
   
   (require 'pollen-inner) ; provides 'doc
   
   ;; reduce text to simplest representation: a single ouput string
   (define main (apply string-append (map ->string (flatten (trim (->list doc) whitespace?)))))
   (provide main (all-from-out 'pollen-inner))

   (module+ main
;     (displayln ";-------------------------")
;     (displayln (string-append "; pollen 'main"))     
;     (displayln ";-------------------------")
     (display main))))
