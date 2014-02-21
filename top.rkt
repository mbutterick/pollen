#lang racket/base

;; Changes the default behavior of #%top.
;; Unbound identifiers are allowed, and treated as the 
;; tag in a txexpr (with the rest of the expression treated as the body)
;; To suppress this behavior, use bound/c to wrap any name.
;; If that name isn't already defined, you'll get the usual syntax error.

(require (for-syntax racket/base))

(provide (except-out (all-defined-out) top~)
         (rename-out (top~ #%top)))

;; Allow tag attributes to be specified as follows:
;; @foo['shape: "square" 'color: "red"]{hello}
(define-syntax-rule (top~ . id)
  (λ x 
    (define attrs null)
    (define elements 
      (let chomp ([x x])
        (define result+regexp (and ((length x) . >= . 2) 
                                         (symbol? (car x)) 
                                         ;; accept strings only
                                         ;; numbers are difficult because they don't parse as cleanly.
                                         ;; string will read as a string even if there's no space to the left.
                                         (or (string? (cadr x))) 
                                         ;; Looking for symbol ending with a colon
                                         (regexp-match #rx"^(.*?):(.*)$" (symbol->string (car x)))))
        (if result+regexp
            (begin
               ; reuse result value cadr is first group in match. 
              (set! attrs (cons (list (string->symbol (cadr result+regexp))(cadr x)) attrs))
              (chomp (cddr x)))
            x)))
    
    `(id ,@(if (equal? attrs null) null (list (reverse attrs))) ,@elements)))


(define-syntax (bound/c stx)
  (syntax-case stx ()
    [(_ x)
     (if (identifier-binding #'x )
         #'x
         #'(#%top . x))]))
