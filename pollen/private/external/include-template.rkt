#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     syntax/parse)
         (only-in scribble/text/syntax-utils include/text)
         (only-in "output.rkt" output)
         racket/match
         racket/port)

(provide include-template)

;; Adaptation of function in web-server/templates library 
;; to check for binary result and pass it through.
;; Actually patches underlying bug in `output`.

(define (finish result)
  (match result
    [(? bytes? bs) bs]
    ;; list of expressions with byte string in last place.
    ;; infer that user is trying to return a binary as the last value in a template,
    ;; and treat it as a single binary value.
    [(list _ ... (? bytes? bs)) bs]
    [_ (with-output-to-string (λ () (output result)))]))

(define-syntax (include-template stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:command-char command-char:expr)) src:expr)
     (quasisyntax/loc stx
       (finish (include/text #,@(if (attribute command-char)
                                    (list #'#:command-char #'command-char)
                                    empty)
                             src)))]))


