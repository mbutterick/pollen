#lang racket/base
(require racket/match
         racket/list
         "constants.rkt")
(provide (all-defined-out))

;; (string->symbol (format "~a" #\u200B))

(define (attrs? x)
  (match x
    [(list (list (? symbol?) (? string?)) ...) #true]
    [_ #false]))

(define (null-string? x) (equal? x ""))

(define ((spliceable? splicing-tag) x)
  (match x
    [(cons (== splicing-tag eq?) _) #true]
    [_ #false]))

(define (splice x [splicing-tag pollen-splicing-tag])
  ;  (listof txexpr-elements?) . -> . (listof txexpr-elements?))
  (let loop ([x x])
    (if (list? x) ; don't exclude `attrs?` here, because it will exclude valid splice input like '((@ "foo"))
        (append-map (λ (x)
                      ; drop the splice-signal from front with `rest`
                      ; don't recur on attributes, so null strings are not spliced within
                      (define proc (if ((spliceable? splicing-tag) x) rest list))
                      (proc (if (attrs? x) x (loop x))))
                    (filter-not null-string? x))
        x)))

(module+ test
  (require rackunit)
  (check-equal? (splice `((div 1 (,pollen-splicing-tag 2 "" (,pollen-splicing-tag 3 (div 4 (,pollen-splicing-tag 5))) 6) "" 7)))
                '((div 1 2 3 (div 4 5) 6 7)))
  (check-equal? (splice `((,pollen-splicing-tag 1 (,pollen-splicing-tag 2 "" (,pollen-splicing-tag 3 (div 4 (,pollen-splicing-tag 5))) 6) "" 7)))
                '(1 2 3 (div 4 5) 6 7))
  (check-equal? (splice `((,pollen-splicing-tag "foo" "" "bar"))) '("foo" "bar"))
  (check-equal? (splice null) null)
  (check-equal? (splice '(a ((href "")(foo "bar")) "zam")) '(a ((href "")(foo "bar")) "zam"))
  (check-equal? (splice `((,pollen-splicing-tag "str"))) '("str")))


;; this will strip all empty lists.
;; in practice, they would only appear in attrs position 
(define (strip-empty-attrs x)
  (let loop ([x x])
    (if (pair? x)
        (map loop (filter-not null? x))
        x)))

(module+ test
  (check-equal? (strip-empty-attrs '(p ())) '(p))
  (check-equal? (strip-empty-attrs '(p () "foo")) '(p "foo"))
  (check-equal? (strip-empty-attrs '(p () (em () "foo") "bar")) '(p (em "foo") "bar")))

;; used with pollen/markup to suppress void arguments,
;; consistent with how pollen/pre and pollen/markdown handle them
(define (remove-voids x)
  (let loop ([x x])
    (if (pair? x)
        (map loop (filter-not void? x))
        x)))

(module+ test
  (check-equal? (remove-voids (list 1 2 3 (void))) '(1 2 3))
  (check-equal? (remove-voids (list 1 (void) 2 3 (list 4 5 6 (void)))) '(1 2 3 (4 5 6)))
  (check-equal? (remove-voids (void)) (void)))