#lang racket/base

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [replacement-module-begin #%module-begin]))

(define-syntax-rule (replacement-module-begin body ...)
  (#%module-begin
   (module inner pollen/lang/doclang_raw
     main
     (λ(x) (apply string-append (cdr x))) ;; chop first linebreak with cdr
     ()
     (require pollen/main-helper)
     (require-project-require-files)
     (provide (all-defined-out))
     
     body ...)
   
   (require 'inner)
   (provide (all-from-out 'inner))
   
   (module+ main
     (display main))))
   