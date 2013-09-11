#lang racket/base

(require (only-in (planet mb/pollen/readability) ->list)
         (only-in (planet mb/pollen/tools) trim)
         (only-in (planet mb/pollen/predicates) whitespace?))

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [module-begin #%module-begin]))

(require (only-in scribble/text output))

(define-syntax-rule (module-begin expr ...)
  (#%module-begin
   
   ; We want our module language to support require & provide
   ; which are only supported at the module level, so ...
   ; create a submodule to contain the input
   ; and export as needed
   
   ; doclang2_raw is a clone of scribble/doclang2 with decode disabled
   ; helpful because it collects & exports content via 'doc
   (module pollen-inner (planet mb/pollen/lang/doclang2_raw)
     (require (planet mb/pollen/tools) (planet mb/pollen/main-helper))
     (require-and-provide-extras) ; brings in the project require files
     
     expr ...) ; body of module 
   
   (require 'pollen-inner) ; provides 'doc
   
   (define text (trim (->list doc) whitespace?)) ; if single line, text will be a string
   
   (provide text (all-from-out 'pollen-inner))
   
   (output text)))