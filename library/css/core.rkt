#lang racket/base
(require racket/string racket/list racket/contract)
(require (planet mb/pollen/readability))

(provide (all-defined-out)
         (all-from-out racket/string racket/list racket/contract (planet mb/pollen/readability)))

(define css-property-prefixes '("-moz-" "-webkit-" "-o-" "-ms-" ""))

(define (join-css-strings properties)
  (define line-ending ";\n")
  (define out-string (string-join properties line-ending))
  (if (ends-with? out-string line-ending) ; might already have the line ending, so don't duplicate it
      out-string
      (string-append out-string line-ending)))

(define (make-css-string p v)
    (string-join (list (->string p) (->string v)) ": "))

(define (make-css-strings property-prefixes property-suffix values)
  ; general function for creating groups of css properties
  ; with browser prefixes and one value
  (define (map-suffix suffix prefixes)
    (map (λ(prefix) (string-append prefix suffix)) prefixes))
  
  
  (define properties (map-suffix property-suffix property-prefixes))
  
  ; if single value provided, convert to list of values
  ; so that it will work with map in the next step
  (when (not (list? values))
    (set! values (make-list (len properties) values)))
  
  (map make-css-string properties values))

