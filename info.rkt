#lang info
(define collection "pollen")
(define deps '("base" "txexpr" "sugar" "markdown"))
(define scribblings '(("scribblings/pollen.scrbl" (multi-page))))
(define raco-commands '(("pollen" pollen/raco "issue Pollen command" #f)))
