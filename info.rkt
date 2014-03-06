#lang info
(define collection "pollen")
(define scribblings '(("scribblings/pollen.scrbl" ())))
(define deps '("txexpr" "sugar" "markdown"))
(define raco-commands '(("pollen" pollen/raco "issue Pollen command" #f)))
