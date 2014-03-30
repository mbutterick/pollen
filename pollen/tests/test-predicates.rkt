#lang racket/base
(require "../predicates.rkt" racket/list sugar)
(module+ test (require rackunit))


(module+ test
  (check-true (meta-xexpr? '(meta "key" "value")))
  (check-false (meta-xexpr? '(meta "key" "value" "foo")))
  (check-false (meta-xexpr? '(meta))))


(module+ test
  (check-true (block-xexpr? '(p "foo")))
  (check-true (block-xexpr? '(div "foo")))
  (check-false (block-xexpr? '(em "foo")))
  (check-false (block-xexpr? '(barfoo "foo"))))


(module+ test
  (check-equal? (hash-ref (count-incidence '(a b c d b c)) 'b) 2)
  (check-equal? (hash-ref (count-incidence '(a b c d b c)) 'a) 1))

(module+ test
  (check-true (members-unique? '(1 2 3)))
  (check-false (members-unique? '(1 2 2)))
  (check-true (members-unique? (->vector '(1 2 3))))
  (check-false (members-unique? (->vector '(1 2 2))))
  (check-true (members-unique? "fob"))
  (check-false (members-unique? "foo")))

#|
(module+ test
  (check-true (whitespace? " "))
  (check-false (whitespace? "foo"))
  (check-false (whitespace? 'foo))
  (check-false (whitespace? #\Ø))
  (check-false (whitespace? " ")) ; a nonbreaking space. todo: why is this so?
  (check-true (whitespace? "\n \n"))
  (check-true (whitespace? (list "\n" " " "\n")))
  (check-true (whitespace? (list "\n" " " "\n" (list "\n" "\n")))))|#