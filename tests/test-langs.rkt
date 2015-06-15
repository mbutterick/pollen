#lang racket/base
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
(define-runtime-path test.ptree "../test-support/test.ptree")
(define-runtime-path test.html.pm "../test-support/test.html.pm")
(define-runtime-path test.html.pmd "../test-support/test.html.pmd")
(define-runtime-path test.html.pp "../test-support/test.html.pp")
(define-runtime-path test.no-ext "../test-support/test.no-ext")


;; `find-exe` avoids reliance on $PATH of the host system
(define racket-path (find-exe))
(when racket-path
  (define (run path)
    (define cmd-string (format "~a ~a" racket-path path))
    (with-output-to-string (λ() (system cmd-string))))
  (check-equal? (run test.ptree) "'(pagetree-root test ====)")
  (check-equal? (run test.html.pm) "'(root \"test\" \"\\n\" \"====\")")
  (check-equal? (run test.html.pmd) "'(root (h1 ((id \"test\")) \"test\"))")
  (check-equal? (run test.html.pp) "test\n====")
  (check-equal? (run test.no-ext) "test\n===="))
