#lang racket/base
(require racket/date)
(require racket/string)
(require racket/format)


(provide (all-defined-out))

; todo: contracts, tests, docs

(require (prefix-in williams: (planet williams/describe/describe)))

(define (describe x)
  (williams:describe x)
  x)

; debug utilities
(define months (list "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define (message . items)
  (define (zero-fill str count)
    (set! str (~a str))
    (if (> (string-length str) count)
        str
        (string-append (make-string (- count (string-length str)) #\0) str)))
  
  (define (make-date-string)
    (define date (current-date))
    (define date-fields (map (λ(x) (zero-fill x 2)) (list
                                   (date-day date)
                                   (list-ref months (sub1 (date-month date)))
                                   (modulo (date-hour date) 12)
                                   (date-minute date)
                                   (date-second date)
                                ;   (if (< (date-hour date) 12) "am" "pm")
                                   )))  
    (apply format "[~a-~a ~a:~a:~a]" date-fields))
  (displayln (string-join `(,(make-date-string) ,@(map (λ(x)(if (string? x) x (~v x))) items))) (current-error-port)))


; report the current value of the variable, then return it
(define-syntax-rule (report var)
  (begin 
    (message 'var "=" var) 
    var))