#lang racket/base
(require "private/main-base.rkt")

(define+provide-module-begin-in-mode default-mode-pagetree)

(module reader "private/reader-base.rkt"
  default-mode-pagetree)
