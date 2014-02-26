#lang racket/base
(require (only-in scribble/reader make-at-reader)
         pollen/world)

(provide (all-defined-out) (all-from-out pollen/world))

(define read-inner
  (make-at-reader #:command-char EXPRESSION_DELIMITER #:syntax? #t #:inside? #t))

(define (make-pollen-read pollen-read-syntax-proc) 
  (λ(p)
    (syntax->datum
     (pollen-read-syntax-proc (object-name p) p))))

(define (make-pollen-read-syntax reader-mode)
  (λ (path-string p)
    (define file-contents (read-inner path-string p))
    (datum->syntax file-contents 
                   `(module pollen-lang-module pollen 
                      (define reader-mode ',reader-mode)
                      ,@file-contents) 
                   file-contents)))

(define-syntax-rule (make-reader-with-mode mode)
  (begin
    (define reader-mode mode)
    (define pollen-read-syntax (make-pollen-read-syntax reader-mode))
    (define pollen-read (make-pollen-read pollen-read-syntax))
    (provide (rename-out [pollen-read read] [pollen-read-syntax read-syntax]) read-inner)))
