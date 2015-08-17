#lang at-exp racket/base
(require rackunit racket/port racket/system racket/runtime-path compiler/find-exe)

(module test-default pollen
  "hello world")
(require (prefix-in default: 'test-default))
(check-equal? default:doc "hello world")


(module test-pre pollen/pre
  "hello world")
(require (prefix-in pre: 'test-pre))
(check-equal? pre:doc "hello world")


(module test-markup pollen/markup
  "hello world")
(require (prefix-in markup: 'test-markup))
(check-equal? markup:doc '(root "hello world"))


(module test-markdown pollen/markdown
  "hello world")
(require (prefix-in markdown: 'test-markdown))
(check-equal? markdown:doc '(root (p () "hello world")))


(module test-ptree pollen/ptree
  '(index (brother sister)))
(require (prefix-in ptree: 'test-ptree))
(check-equal? ptree:doc '(pagetree-root (index (brother sister))))


;; define-runtime-path only allowed at top level
(define-runtime-path test.ptree "data/test.ptree")
(define-runtime-path test.html.pm "data/test.html.pm")
(define-runtime-path test-import.html.pm "data/test-import.html.pm")
(define-runtime-path test.html.pmd "data/test.html.pmd")
(define-runtime-path test.html.pp "data/test.html.pp")
(define-runtime-path test.no-ext "data/test.no-ext")


;; `find-exe` avoids reliance on $PATH of the host system
(define racket-path (find-exe))
(when racket-path
  (define (run path)
    (define cmd-string (format "'~a' ~a" racket-path path))
    (with-output-to-string (λ() (system cmd-string))))
  (check-equal? (run test.ptree) "'(pagetree-root test ====)")
    (check-equal? (run test.html.pm) @string-append{'(root "test" "\n" "====")})
  ;; todo: this one's a little weird. Pollen-to-Pollen require prints the result of required file on import.
  (check-equal? (run test-import.html.pm) @string-append{'(root "This is sample 01.")'(root "test" "\n" "====" "\n" (root "This is sample 01."))})
  (check-equal? (run test.html.pmd) "'(root (h1 ((id \"test\")) \"test\"))")
  (check-equal? (run test.html.pp) "test\n====")
  (check-equal? (run test.no-ext) "test\n===="))
