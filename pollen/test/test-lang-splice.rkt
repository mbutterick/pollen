#lang racket/base
(require rackunit)

(module markup pollen/markup
  "Hello" (when #t (@ "Splice")) "" (when/splice #t "Splice") "World")
(require (prefix-in markup: 'markup))
(check-equal? markup:doc '(root "Hello" "Splice" "Splice" "World"))

(module pre pollen/pre
  "Hello" (when #t (@ "Splice")) "" (when/splice #t "Splice") "World")
(require (prefix-in pre: 'pre))
(check-equal? pre:doc "HelloSpliceSpliceWorld")

(module markdown pollen/markdown
  "Hello" (when #t (@ "Splice")) "" (when/splice #t "Splice") "World")
(require (prefix-in markdown: 'markdown))
(check-equal? markdown:doc '(root (p "HelloSpliceSpliceWorld")))
