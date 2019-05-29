#lang racket/base
(require racket/match
         racket/list)
(provide (all-defined-out))

(define (split-metas x [meta-key 'define-meta])
  (let loop ([x ((if (syntax? x) syntax->datum values) x)])
    (match x
      [(list (== meta-key eq?) key)
       (raise-argument-error meta-key "meta value missing" key)]
      [(list (== meta-key eq?) key val)
       (unless (symbol? key)
         (raise-argument-error meta-key "valid meta key" key))
       (list (cons key val))]
      [(? list? xs) (append-map loop xs)]
      [_ null])))

(module+ test
  (require rackunit)
  (check-equal? (split-metas 'root) null)
  (check-equal? (split-metas '(root)) null)
  (check-exn exn:fail:contract? (λ () (split-metas '(root (define-meta 42 "bar")))))
  (check-equal? (split-metas '(root (div #:kw #f (define-meta foo "bar") "hi") "zim" (define-meta foo "boing") "zam")) '((foo . "bar") (foo . "boing")))
  (check-equal? (split-metas '(root (div #:kw #f (define-meta foo 'bar) "hi") "zim" (define-meta foo 'boing) "zam")) '((foo . 'bar) (foo . 'boing)))
  (check-equal? (split-metas #'(root (define-meta dog "Roxy") (define-meta dog "Lex"))) '((dog . "Roxy") (dog . "Lex")))
  (check-equal? (split-metas #'(root (define-meta dog "Roxy") (div (define-meta dog "Lex")))) '((dog . "Roxy") (dog . "Lex"))))