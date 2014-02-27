#lang racket/base
(require racket/date racket/string)
(require sugar/debug sugar/define "world.rkt")

(provide  (all-from-out sugar/debug))

; todo: contracts, tests, docs


; debug utilities
(define months (list "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define last-message-time #f)
(define (seconds-since-last-message)
  (define now (current-seconds))
  (define then last-message-time)
  (set! last-message-time now)
  (if then      
      (- now then)
      "0"))

(define (zero-fill str count)
  (set! str (format "~a" str))
  (if (> (string-length str) count)
      str
      (string-append (make-string (- count (string-length str)) #\0) str)))

(define+provide (make-datestamp)
  (define date (current-date))
  (define date-fields (map (λ(x) (zero-fill x 2)) 
                           (list
                            (date-day date)
                            (list-ref months (sub1 (date-month date)))
                            (date-year date)
                            )))
  (string-join date-fields "-"))

(define+provide (make-timestamp)  
  (define date (current-date))
  (define time-fields (map (λ(x) (zero-fill x 2)) 
                           (list
                            ; (date-day date)
                            ; (list-ref months (sub1 (date-month date)))
                            (if (<= (date-hour date) 12)
                                (date-hour date) ; am hours + noon hour
                                (modulo (date-hour date) 12)) ; pm hours after noon hour
                            (date-minute date)
                            (date-second date)
                            
                            
                            )))  
  (string-append (string-join time-fields ":") (if (< (date-hour date) 12) "am" "pm")))

(define (make-debug-timestamp)
  (format "[~a ∆~as]" (make-timestamp) (seconds-since-last-message)))



;; todo: consolidate these two message functions
(define+provide (basic-message . items)
  (displayln (string-join `(,@(map (λ(x)(if (string? x) x (format "~v" x))) items))) (current-error-port)))

(define+provide (message . items)
  (apply message-threshold world:threshold-normal items))

(define+provide (message-threshold threshold . items)
  (when (threshold . <= . (world:current-message-threshold))
    (displayln (string-join `(,(make-debug-timestamp) ,@(map (λ(x)(if (string? x) x (format "~v" x))) items))) (current-error-port))))

(define (exn+stack->string exn)
  (string-append 
   (string-append "Exception: " (exn-message exn)) 
   "\n"
   "Stack:\n"
   (string-join
    (map (lambda (x)
           (format "'~a' ~a ~a"
                   (if (car x) (car x) "")
                   (if (cdr x) (srcloc-source (cdr x)) "")
                   (if (cdr x) (srcloc-line (cdr x)) "")))
         (continuation-mark-set->context (exn-continuation-marks exn)))
    "\n")))

(define (display-stack-trace exn)
  (displayln (exn+stack->string exn)))