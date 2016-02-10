#lang racket/base
(require "private/main-base.rkt")

(define+provide-module-begin-in-mode default-mode-preproc) ; because default mode in submodule is preproc

(module reader "private/reader-base.rkt"
  default-mode-auto)