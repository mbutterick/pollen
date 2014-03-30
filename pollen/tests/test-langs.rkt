#lang racket/base



(module test-default pollen
  "hello world")

(require (prefix-in default: 'test-default))

default:doc ; should be "hello world"


(module test-pre pollen/pre
  "hello world")

(require (prefix-in pre: 'test-pre))

pre:doc  ; should be "hello world"


(module test-markup pollen/markup
  "hello world")

(require (prefix-in markup: 'test-markup))

markup:doc ; should be '(root "hello world")

(module test-markdown pollen/markdown
  "hello world")

(require (prefix-in markdown: 'test-markdown))

markdown:doc ; should be '(root (p "hello world"))


(module test-ptree pollen/ptree
  '(index (brother sister)))

(require (prefix-in ptree: 'test-ptree))

ptree:doc ; should be '(pagetree-root (index (brother sister)))


(begin 
 (require racket/rerequire)
  (dynamic-rerequire (string->path "/Users/mb/git/bpt/test.html.pm") #:verbosity 'reload)
  (dynamic-require (string->path "/Users/mb/git/bpt/test.html.pm") 'doc))



(eval 
 '(begin 
    (require racket/rerequire)
    (dynamic-rerequire (string->path "/Users/mb/git/bpt/test.html.pm") #:verbosity 'reload)
    (dynamic-require (string->path "/Users/mb/git/bpt/test.html.pm") 'doc)) (make-base-namespace))

