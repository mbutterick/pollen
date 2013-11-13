#lang racket/base
(require "core.rkt")

(struct cssq (num unit)   
  #:methods gen:custom-write
  [(define write-proc 
     (λ(x port mode) (display (format "~a~a" (cssq-num x) (cssq-unit x)) port)))])

(define (string->cssq str)
  (apply cssq 
         (let ([str (string-trim str "s" #:left? #f)])
           (regexp-split #px"(?<=[.\\d])(?=[A-Za-z])" str))))


(define (css+ left right)
  (string->cssq (format "~a~a" (apply + (map string->number (list (cssq-num left) (cssq-num right))))(cssq-unit left))))

(module+ main
  (define q1 (string->cssq "51.2rems"))
  (define q2 (string->cssq "10rem"))
  (css+ q1 q2))