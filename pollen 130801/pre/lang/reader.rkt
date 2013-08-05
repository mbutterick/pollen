#lang racket/base
(require (only-in scribble/reader make-at-reader)
         (only-in (planet mb/pollen/world) POLLEN_EXPRESSION_DELIMITER))

(provide (rename-out [mb-read read]
                     [mb-read-syntax read-syntax])
         read-inner
         )

(define read-inner
  (make-at-reader #:command-char POLLEN_EXPRESSION_DELIMITER
                  #:syntax? #t
                  #:inside? #t))

(define (mb-read p)
  (syntax->datum
   (mb-read-syntax (object-name p) p)))

(define (make-output-datum i)
  `(module lang-module (planet mb/pollen/main-pre) 
     ,@i))

(define (mb-read-syntax name p)
  (define i (read-inner name p))
  (datum->syntax i 
                 (make-output-datum i)
                 i))
