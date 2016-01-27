#lang racket/base
(provide (all-defined-out))

(define (splice x [splicing-tag '@])
  (let loop ([x x])
    (if (list? x)
        (apply append
               (map (λ(xi) (let ([proc (if (and (pair? xi) (eq? (car xi) splicing-tag))
                               cdr ; expose elements
                               list)]) ; wrap in list 
                            (proc (loop xi)))) x))
        x)))

(module+ test
  (require rackunit)
  (check-equal? (splice '(@ 1 (@ 2 (@ 3 (div 4 (@ 5))) 6) 7))
                '(@ 1 2 3 (div 4 5) 6 7))
  (check-equal? (splice '((@ "foo" "bar"))) '("foo" "bar"))
  (check-equal? (splice '(@ "foo" "bar")) '(@ "foo" "bar")) ; this is correct, for composable behavior
  (check-equal? (splice null) null))