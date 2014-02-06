#lang racket/base
(require (only-in scribble/reader make-at-reader)
         (only-in "../world.rkt" EXPRESSION_DELIMITER)
         (only-in "../file-tools.rkt" preproc-source?))

(provide (rename-out [mb-read read]
                     [mb-read-syntax read-syntax])
         read-inner
         )

(define read-inner
  (make-at-reader #:command-char EXPRESSION_DELIMITER
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
                 ;; select pollen dialect based on file type
                 `(module pollen-lang-module ,(if (preproc-source? path-string)
                                                  'pollen/main-preproc
                                                  'pollen/main)
                    ,@i)
                 i))
