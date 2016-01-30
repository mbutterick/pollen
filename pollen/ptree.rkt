#lang racket/base
(require "private/main-base.rkt")

(define+provide-module-begin-in-mode setup:default-mode-pagetree)

(module reader racket/base
  (require pollen/private/reader-base)
  (define+provide-reader-in-mode setup:default-mode-pagetree))
