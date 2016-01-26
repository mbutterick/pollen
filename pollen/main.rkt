#lang racket/base
(require "private/main-base.rkt")

(define+provide-module-begin-in-mode world:mode-preproc) ; because default mode in submodule is preproc

(module reader racket/base
  (require pollen/private/reader-base)
  (define+provide-reader-in-mode world:mode-auto)) ; because default mode in file is auto
