#lang racket/base
(require rackunit racket/port racket/system racket/runtime-path)


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

(define (run path)
  (define cmd-string (format "racket ~a" path))
  (with-output-to-string (λ() (system cmd-string))))

(define-runtime-path test.ptree "test.ptree")
(check-equal? (run test.ptree) "'(pagetree-root test ====)")
(define-runtime-path test.html.pm "test.html.pm")
(check-equal? (run test.html.pm) "'(root \"test\" \"\\n\" \"====\")")
(define-runtime-path test.html.pmd "test.html.pmd")
(check-equal? (run test.html.pmd) "'(root (h1 ((id \"test\")) \"test\"))")
(define-runtime-path test.html.pp "test.html.pp")
(check-equal? (run test.html.pp) "test\n====")
(define-runtime-path test.no-ext "test.no-ext")
(check-equal? (run test.no-ext) "test\n====")
