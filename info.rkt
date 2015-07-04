#lang info
(define collection "pollen")
(define deps '("base" "txexpr" "sugar" ("markdown" #:version "0.18") "htdp"
               "at-exp-lib" "html-lib" "rackjure" "web-server-lib" "scribble-text-lib" "rackunit-lib"))
(define build-deps '("plot-gui-lib" "scribble-lib" "racket-doc" "plot-doc" "scribble-doc" "slideshow-doc" "web-server-doc"))
(define update-implies '("txexpr" "sugar"))
(define scribblings '(("scribblings/pollen.scrbl" (multi-page))))
(define raco-commands '(("pollen" (submod pollen/command raco) "issue Pollen command" #f)))
(define compile-omit-paths '("tests"))
(define test-omit-paths '("tests/data"))
