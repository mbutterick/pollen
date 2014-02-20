#lang racket/base
(require (only-in scribble/reader make-at-reader))

(provide (rename-out [pollen-read read] [pollen-read-syntax read-syntax]) read-inner)

(define read-inner
  (make-at-reader #:command-char #\◊
                  #:syntax? #t
                  #:inside? #t))

(define (pollen-read p)
  (syntax->datum
   (pollen-read-syntax (object-name p) p)))

(define (make-output-datum i)
  `(module pollen-lang-module pollen 
     ,@i))


(define (pollen-read-syntax path-string p)
  (define file-contents (read-inner path-string p))
  (define file-ext (car (regexp-match #px"\\w+$" (path->string path-string))))
  (datum->syntax file-contents 
                 `(module pollen-lang-module ,(if (member file-ext (list "pd" "ptree")) 
                                                  'pollen/main 
                                                  'pollen/main-preproc) 
                    ,@file-contents)
                 file-contents))
