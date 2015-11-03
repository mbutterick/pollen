#lang pollen/mode racket/base
(require rackunit racket/string)

(define (proc)
    (apply string-join (string-split ◊string-append{foo bar zam}) ◊'{X}))

(check-equal? (proc) "fooXbarXzam")