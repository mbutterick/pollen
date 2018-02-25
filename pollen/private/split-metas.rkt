#lang racket/base
(provide (all-defined-out))

(define (split-metas x meta-key)
  (apply hasheq
         (let loop ([x (if (syntax? x) (syntax->datum x) x)])
           (cond
             [(list? x) (cond
                          [(and (= (length x) 3) (eq? (car x) meta-key))
                           (unless (symbol? (cadr x))
                             (raise-argument-error 'define-meta "valid meta key" (cadr x)))
                           (cdr x)] ; list with meta key and meta value
                          [else (apply append (map loop x))])]
             [else null]))))


(module+ test
  (require rackunit)
  (check-equal? (split-metas 'root 'define-meta) (hasheq))
  (check-equal? (split-metas '(root) 'define-meta) (hasheq))
  (check-exn exn:fail:contract? (λ () (split-metas '(root (define-meta 42 "bar")) 'define-meta)))
  (check-equal? (split-metas '(root (div #:kw #f (define-meta foo "bar") "hi") "zim" (define-meta foo "boing") "zam") 'define-meta) '#hasheq((foo . "boing")))
  (check-equal? (split-metas '(root (div #:kw #f (define-meta foo 'bar) "hi") "zim" (define-meta foo 'boing) "zam") 'define-meta) '#hasheq((foo . 'boing)))
  (check-equal? (split-metas #'(root (define-meta dog "Roxy") (define-meta dog "Lex")) 'define-meta) '#hasheq((dog . "Lex")))
  (check-equal? (split-metas #'(root (define-meta dog "Roxy") (div (define-meta dog "Lex"))) 'define-meta) '#hasheq((dog . "Lex"))))