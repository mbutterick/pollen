#lang info

(define scribblings '(("scribblings/pollen.scrbl" (multi-page))))
(define raco-commands '(("pollen" (submod pollen/private/command raco) "issue Pollen command" #f)))
(define compile-omit-paths '("test" "tools" "server-extras" "scribblings/third-tutorial-files"))
;; it's redundant to test "pollen.scrbl" because it incorporates the other scribble sources by reference
;; don't test app.rkt because it launches the gui
(define test-omit-paths '("test/data" "tools" "server-extras" "scribblings/third-tutorial-files" "scribblings/pollen.scrbl" "app"))
;; don't put #"p" in this list because it's not a #lang
(define module-suffixes '(#"pp" #"pm" #"pmd" #"ptree"))

(define gracket-launcher-names '("Pollen"))
(define gracket-launcher-libraries '("app/app.rkt"))