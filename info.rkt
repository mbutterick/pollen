#lang info
(define collection "pollen")
(define deps '(("base" #:version "6.0") "txexpr" "sugar" ("markdown" #:version "0.18") "htdp"
               "at-exp-lib" "html-lib" "rackjure" "web-server-lib"))
(define build-deps '("plot-gui-lib" "rackunit-lib" "scribble-lib"))
(define update-implies '("txexpr" "sugar"))
(define scribblings '(("scribblings/pollen.scrbl" (multi-page))))
(define raco-commands '(("pollen" pollen/raco "issue Pollen command" #f)))
(define compile-omit-paths '("tests" "raco.rkt"))
