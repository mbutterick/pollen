#lang racket
(require pollen/unstable/convert pollen/unstable/typography)
(provide (except-out (all-from-out racket) #%module-begin))
(provide (rename-out [mb #%module-begin]))
(define-syntax (mb stx)
  (syntax-case stx ()
    [(_ ARG ...)
     #'(#%module-begin (displayln (html->pollen (regexp-replace* #px"\\s+" (string-join (list ARG ...) "") " "))))]))

(module reader syntax/module-reader
  pollen/unstable/convert/html-to-pollen
  #:read h2p-read
  #:read-syntax h2p-read-syntax
  #:whole-body-readers? #t ;; need this to make at-reader work
  (require scribble/reader)
  
  (define (h2p-read p) (syntax->datum (h2p-read-syntax (object-name p) p)))
  
  (define (h2p-read-syntax path-string p)
    (define h2p-at-reader (make-at-reader
                            #:syntax? #t 
                            #:inside? #t))
    (h2p-at-reader path-string p)))