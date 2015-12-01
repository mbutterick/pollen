#lang info
(define collection "pollen")
(define deps '("base" "txexpr" "sugar" ("markdown" #:version "0.18") "htdp"
               "at-exp-lib" "html-lib" "rackjure" "web-server-lib" "scribble-text-lib" "rackunit-lib"
               "gui-lib"))
(define build-deps '("plot-gui-lib" "scribble-lib" "racket-doc" "rackunit-doc" "plot-doc" "scribble-doc" "slideshow-doc" "web-server-doc"))
(define update-implies '("txexpr" "sugar"))
(define scribblings '(("scribblings/pollen.scrbl" (multi-page))))
(define raco-commands '(("pollen" (submod pollen/command raco) "issue Pollen command" #f)))
(define compile-omit-paths '("test" "tools" "server-extras" "scribblings/third-tutorial-files"))
;; it's redundant to test "pollen.scrbl" because it incorporates the other scribble sources by reference
(define test-omit-paths '("test/data" "tools" "server-extras" "scribblings/third-tutorial-files" "scribblings/pollen.scrbl"))
(define module-suffixes '(#"pp" #"pm" #"pmd" #"ptree"))
