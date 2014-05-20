#lang racket/base
(require pollen/convert)

(provide $ $$)

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

(define first-run #t)

(define (mathjax-wrapper #:delimiter delimiter xs)
  `(mathjax 
    ,@(if first-run ; only need mathjax-config & mathjax-library once on the page
          (begin (set! first-run #f) (map html->xexpr (list mathjax-config mathjax-library))) 
          null) 
    ,(apply string-append `(,delimiter ,@xs ,delimiter)))) 

(define ($ . x)
  (mathjax-wrapper #:delimiter "$" x))


(define ($$ . x)
  (mathjax-wrapper #:delimiter "$$" x))
