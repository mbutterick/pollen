#lang racket/base
(require racket/match
         racket/list)
(provide (all-defined-out))

(define (split-metas x meta-key)
  (apply hasheq
         (let loop ([x ((if (syntax? x) syntax->datum values) x)])
           (match x
             [(? list? xs)
              (match xs
                [(list (== meta-key eq?) key val)
                 (unless (symbol? key)
                   (raise-argument-error 'define-meta "valid meta key" key))
                 (list key val)]
                [_ (append-map loop xs)])]
             [_ null]))))

(module+ test
  (require rackunit)
  (check-equal? (split-metas 'root 'define-meta) (hasheq))
  (check-equal? (split-metas '(root) 'define-meta) (hasheq))
  (check-exn exn:fail:contract? (λ () (split-metas '(root (define-meta 42 "bar")) 'define-meta)))
  (check-equal? (split-metas '(root (div #:kw #f (define-meta foo "bar") "hi") "zim" (define-meta foo "boing") "zam") 'define-meta) '#hasheq((foo . "boing")))
  (check-equal? (split-metas '(root (div #:kw #f (define-meta foo 'bar) "hi") "zim" (define-meta foo 'boing) "zam") 'define-meta) '#hasheq((foo . 'boing)))
  (check-equal? (split-metas #'(root (define-meta dog "Roxy") (define-meta dog "Lex")) 'define-meta) '#hasheq((dog . "Lex")))
  (check-equal? (split-metas #'(root (define-meta dog "Roxy") (div (define-meta dog "Lex"))) 'define-meta) '#hasheq((dog . "Lex"))))