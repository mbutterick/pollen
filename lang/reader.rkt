#lang racket/base
(require (only-in scribble/reader make-at-reader))

(provide (rename-out [mb-read read] [mb-read-syntax read-syntax]) read-inner)

(define read-inner
  (make-at-reader #:command-char #\◊
                  #:syntax? #t
                  #:inside? #t))

(define (mb-read p)
  (syntax->datum
   (mb-read-syntax (object-name p) p)))

(define (make-output-datum i)
  `(module pollen-lang-module pollen 
     ,@i))


(define (mb-read-syntax path-string p)
  (define i (read-inner path-string p)) 
  (datum->syntax i 
                 `(module pollen-lang-module pollen/main-preproc ,@i)
                 i))
