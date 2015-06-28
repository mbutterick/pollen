#lang racket/base
(provide (all-defined-out))

(define (root . xs)
  `(rootover ,@xs))

(define local:pollen-version "42")

(define local:preproc-source-ext 'ppover)
(define local:markup-source-ext 'pmover)
(define local:markdown-source-ext 'pmdover)
(define local:null-source-ext 'p)
(define local:pagetree-source-ext 'ptreeover)

(define local:command-char #\∆)
(define local:main-export 'docover)
(define local:meta-export 'metasover)
(define local:meta-tag-name 'metaover)