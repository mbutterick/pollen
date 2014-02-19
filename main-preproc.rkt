#lang racket/base

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [module-begin #%module-begin]))

(define-syntax-rule (module-begin expr ...)
  (#%module-begin
   (module inner pollen/lang/doclang_raw
     main
     (λ(x) (apply string-append (cdr x))) ;; chop first linebreak off
     ()
     (provide (all-defined-out))
     
     expr ...)
   
   (require 'inner)
   (provide (all-from-out 'inner))
   
   (module+ main
     (display main))))
   