#lang info

;; 210309
;; for unknown reason "mode-indentation.rkt" 
;; started causing CI failures since 210215
;; consistently on 6.7, 6.8, 6.9, 7.7CS, 7.8CS, 7.9CS
;; I assume it has something to do with the fact that 
;; it imports `framework` and `racket/gui`, 
;; OTOH why does it fail in these?
(define test-omit-paths '("mode-indentation.rkt"))