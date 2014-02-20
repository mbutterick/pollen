#lang racket/base

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [new-module-begin #%module-begin]))

(define-syntax-rule (new-module-begin body ...)
  (#%module-begin
   (module inner pollen/lang/doclang_raw
     main-raw
     (λ(x) (apply string-append (cdr x))) ;; chop first linebreak with cdr
     ()
     (require pollen/main-helper)
     (require-project-require-files)
     (provide (all-defined-out))
     
     body ...)
   
   (require 'inner)
   (define main main-raw)
   (provide (all-from-out 'inner) main)
   
   (module+ main
     (display main))))
   