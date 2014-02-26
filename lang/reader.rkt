#lang racket/base

(require pollen/lang/reader-base)

(provide (rename-out [pollen-read read] [pollen-read-syntax read-syntax]) read-inner)

(define reader-mode 'auto)
(define pollen-read-syntax (make-pollen-read-syntax reader-mode))
(define pollen-read (make-pollen-read pollen-read-syntax))