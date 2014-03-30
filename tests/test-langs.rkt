#lang racket/base
(require rackunit)


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


