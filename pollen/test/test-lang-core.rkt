#lang racket/base
(require rackunit)

;; check that automatic imports of pollen/core are present.

(module markup pollen/markup
  (define-meta zing "bam")
  (select 'zing metas))
(require (prefix-in markup: 'markup))
(check-equal? markup:doc '(root "bam"))

(module pre pollen/pre
  (define-meta zing "bam")
  (select 'zing metas))
(require (prefix-in pre: 'pre))
(check-equal? pre:doc "bam")

(module markdown pollen/markdown
  (define-meta zing "bam")
  (select 'zing metas))
(require (prefix-in markdown: 'markdown))
(check-equal? markdown:doc '(root (p () "bam")))
