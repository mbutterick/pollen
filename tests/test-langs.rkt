#lang racket/base
(require rackunit racket/port racket/system)


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


(define (run file)
  (with-output-to-string (λ() (system (format "racket ~a" file)))))

(check-equal? (run "test.ptree") "'(pagetree-root test ====)")
(check-equal? (run "test.html.pm") "'(root \"test\" \"\\n\" \"====\")")
(check-equal? (run "test.html.pmd") "'(root (h1 ((id \"test\")) \"test\"))")
(check-equal? (run "test.html.pp") "test\n====")
(check-equal? (run "test.no-ext") "test\n====")
