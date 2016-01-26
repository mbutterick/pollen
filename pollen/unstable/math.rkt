#lang racket/base
(require "convert.rkt")

(provide $ $$ mathjax-config mathjax-library)

(define mathjax-config #<<HTML
<script type="text/x-mathjax-config">
  MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$']]}});
</script>
HTML
  )

(define mathjax-library #<<HTML
<script type="text/javascript"
  src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
</script>
HTML
  )

(define (mathjax-wrapper #:delimiter delimiter xs)
  `(mathjax ,(apply string-append `(,delimiter ,@xs ,delimiter)))) 

(define ($ . x)
  (mathjax-wrapper #:delimiter "$" x))

(define ($$ . x)
  (mathjax-wrapper #:delimiter "$$" x))
