#lang racket/base
(require (only-in scribble/text/syntax-utils include/text)
         (only-in "output.rkt" output)
         racket/list
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
         
         (let ([result (cond
                         [(bytes? result) result]
                         ;; list of expressions with byte string in last place.
                         ;; infer that user is trying to return a binary as the last value in a template,
                         ;; and treat it as a single binary value.
                         [(and (list? result) (bytes? (last result))) (last result)]
                         [else result])])
           (if (bytes? result) 
               (with-output-to-bytes (λ () (write-bytes result))) 
               (with-output-to-string (λ () (output result)))))))]))

(provide include-template)
