#lang racket/base
(require racket/list)
(provide (all-defined-out))

(define (split-metas tree meta-key)
  (define datums (flatten (if (syntax? tree) (syntax->datum tree) tree)))
  (if (>= (length datums) 3)
      (for/hasheq ([name (in-list datums)]
                   [k (in-list (cdr datums))]
                   [v (in-list (cddr datums))]
                   #:when (eq? name meta-key))
                  (values k v))
      (hasheq)))

(module+ test
  (require rackunit)
  (check-equal? (split-metas '(root) 'define-meta) (hasheq))
  (check-equal? (split-metas '(root (div #:kw #f (define-meta foo "bar") "hi") "zim" (define-meta foo "boing") "zam") 'define-meta) '#hasheq((foo . "boing")))
  (check-equal? (split-metas #'(root (define-meta dog "Roxy") (define-meta dog "Lex")) 'define-meta) '#hasheq((dog . "Lex")))
  (check-equal? (split-metas #'(root (define-meta dog "Roxy") (div (define-meta dog "Lex"))) 'define-meta) '#hasheq((dog . "Lex"))))