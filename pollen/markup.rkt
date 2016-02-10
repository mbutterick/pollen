#lang racket/base
(require "private/main-base.rkt")

(define+provide-module-begin-in-mode default-mode-markup)

(module reader "private/reader-base.rkt"
  default-mode-markup)
