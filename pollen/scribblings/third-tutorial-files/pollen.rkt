#lang racket/base
(require pollen/decode pollen/misc/tutorial txexpr/base)
(define (root . elements)
   (txexpr 'root null (decode-elements elements
     #:txexpr-elements-proc decode-paragraphs
     #:string-proc (compose smart-quotes smart-dashes))))
(provide root)