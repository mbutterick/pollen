#lang racket/base
(provide (all-defined-out))

;; (string->symbol (format "~a" #\u200B))
(define splice-signal-tag '@)

(define (attrs? x)
  (and (list? x)
       (andmap (λ (xi)
                 (and (list? xi)
                      (= (length xi) 2)
                      (symbol? (car xi))
                      (string? (cadr xi)))) x)))


(define (splice x [splicing-tag splice-signal-tag])
  ;  (listof txexpr-elements?) . -> . (listof txexpr-elements?))
  (define spliceable? (λ (x) (and (pair? x) (eq? (car x) splicing-tag))))
  (define not-null-string? (λ (x) (not (and (string? x) (zero? (string-length x))))))
  (let loop ([x x])
    (if (list? x) ; don't exclude `attrs?` here, because it will exclude valid splice input like '((@ "foo"))
        (apply append (map (λ (x) (let ([proc (if (spliceable? x) ; drop the splice-signal from front with `cdr`
                                                  cdr
                                                  list)]
                                        [x (if (not (attrs? x)) ; don't recur on attributes, so null strings are not spliced within
                                               (loop x)
                                               x)])
                                    (proc x))) (filter not-null-string? x)))
        x)))

(module+ test
  (require rackunit)
  (check-equal? (splice `((div 1 (,splice-signal-tag 2 "" (,splice-signal-tag 3 (div 4 (,splice-signal-tag 5))) 6) "" 7)))
                '((div 1 2 3 (div 4 5) 6 7)))
  (check-equal? (splice `((,splice-signal-tag 1 (,splice-signal-tag 2 "" (,splice-signal-tag 3 (div 4 (,splice-signal-tag 5))) 6) "" 7)))
                '(1 2 3 (div 4 5) 6 7))
  (check-equal? (splice `((,splice-signal-tag "foo" "" "bar"))) '("foo" "bar"))
  (check-equal? (splice null) null)
  (check-equal? (splice '(a ((href "")(foo "bar")) "zam")) '(a ((href "")(foo "bar")) "zam"))
  (check-equal? (splice `((,splice-signal-tag "str"))) '("str")))


(define (strip-empty-attrs x)
  (let loop ([x x])
    (if (list? x)
        ;; this will strip all empty lists.
        ;; in practice, they would only appear in attrs position 
        (map loop (filter (λ (x) (not (null? x))) x))
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
        (for/list ([xi (in-list x)]
                   #:unless (void? xi))
                  (loop xi))
        x)))

(module+ test
  (check-equal? (remove-voids (list 1 2 3 (void))) '(1 2 3))
  (check-equal? (remove-voids (list 1 (void) 2 3 (list 4 5 6 (void)))) '(1 2 3 (4 5 6)))
  (check-equal? (remove-voids (void)) (void)))