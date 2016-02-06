#lang racket/base
(provide (all-defined-out))

(define (split-metas tree meta-key)
  (define matches null)
  
  (define (meta? x)  ; meta has form (define-meta key value)
    (and (list? x) (>= (length x) 3) (eq? (car x) meta-key)))
  
  (define rest
    (let loop ([x (if (list? tree) tree (list tree))])
      (cond
        [(meta? x) (set! matches (cons x matches)) #f]
        [(list? x) (filter (λ(x) x) (map loop x))]
        [else x])))
  
  (values (apply hasheq (apply append (reverse (map cdr matches)))) rest))

(module+ test
  (require rackunit)
  (let-values ([(metas rest) (split-metas '(root (div (define-meta foo "bar") "hi") "zim" (define-meta foo "boing") "zam") 'define-meta)])
    (check-equal? metas '#hasheq((foo . "boing")))
    (check-equal? rest '(root (div "hi") "zim" "zam")))
  (let-values ([(metas rest) (split-metas '(root (define-meta dog "Roxy") (define-meta dog "Lex")) 'define-meta)])
    (check-equal? metas '#hasheq((dog . "Lex"))))
  (let-values ([(metas rest) (split-metas '(root (define-meta dog "Roxy") (div (define-meta dog "Lex"))) 'define-meta)])
    (check-equal? metas '#hasheq((dog . "Lex")))))