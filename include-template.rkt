#lang racket/base
(require scribble/text
         (for-syntax racket/base
                     racket/list
                     syntax/parse)
         racket/port)

;; Adaptation of function in web-server/templates library 
;; to check for binary result and pass it through.
;; Actually patches underlying bug in `output`.
(define-syntax (include-template stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:command-char command-char:expr)) p:expr)
     (quasisyntax/loc stx
       (let ([result (include/text #,@(if (attribute command-char)
                                             (list #'#:command-char #'command-char)
                                             empty)
                                      p)])
         (if (bytes? result) 
             (with-output-to-bytes (λ () (write-bytes result))) 
             (with-output-to-string (λ () (output result))))))]))

(provide include-template)
