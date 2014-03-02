#lang info
(define collection "pollen")
(define scribblings '(("scribblings/pollen.scrbl" ())))
(define deps '("txexpr" "sugar" "markdown"))
(define raco-commands '(("pollen" pollen/command "issue Pollen command" #f)))
