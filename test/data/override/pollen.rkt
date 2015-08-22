#lang racket/base
(provide (all-defined-out))

(define (root . xs)
  `(rootover ,@xs))

(module config racket/base
  (provide (all-defined-out))
  (define pollen-version "42")
  
  (define preproc-source-ext 'ppover)
  (define markup-source-ext 'pmover)
  (define markdown-source-ext 'pmdover)
  (define null-source-ext 'p)
  (define pagetree-source-ext 'ptreeover)
  
  (define command-char #\∆)
  (define main-export 'docover)
  (define meta-export 'metasover)
  (define meta-tag-name 'metaover))