#lang racket/base
(require "private/main-base.rkt")

(define+provide-module-begin-in-mode default-mode-preproc)

(module reader "private/reader-base.rkt"
  default-mode-preproc)
