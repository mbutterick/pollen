#lang racket/base
(require (for-syntax racket/base racket/syntax))
(require racket/date racket/string)
(require sugar/debug sugar/define)

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
  (define date-fields (map (λ (x) (zero-fill x 2)) 
                           (list
                            (date-day date)
                            (list-ref months (sub1 (date-month date)))
                            (date-year date)
                            )))
  (string-join date-fields "-"))

(define+provide (make-timestamp)  
  (define date (current-date))
  (define time-fields (map (λ (x) (zero-fill x 2)) 
                           (list
                            ; (date-day date)
                            ; (list-ref months (sub1 (date-month date)))
                            (if (<= (date-hour date) 12)
                                (date-hour date) ; am hours + noon hour
                                (modulo (date-hour date) 12)) ; pm hours after noon hour
                            (date-minute date)
                            (date-second date))))  
  (string-append (string-join time-fields ":") (if (< (date-hour date) 12) "am" "pm")))

(define (make-debug-timestamp)
  (format "[~a ∆~as]" (make-timestamp) (seconds-since-last-message)))

;; creates pollen-logger and associated functions:
;; log-pollen-fatal, log-pollen-error, log-pollen-warning, 
;; log-pollen-info, and log-pollen-debug
(define-logger pollen) 


(define-syntax (make-message-logger-functions stx)
  (syntax-case stx ()
    [(_ stem)
     (with-syntax ([message-stem (format-id stx "message-~a" #'stem)]
                   [log-pollen-stem (format-id stx "log-pollen-~a" #'stem)])
       #'(begin
           ;; does file have particular extension
           (define+provide (message-stem . items)
             (log-pollen-stem (string-join `(,(make-debug-timestamp) ,@(map (λ (x)(if (string? x) x (format "~v" x))) items)))))))]))

(make-message-logger-functions fatal)
(make-message-logger-functions error)
(make-message-logger-functions warning)
(make-message-logger-functions info)
(make-message-logger-functions debug)

(define+provide (message . items)
             (displayln (string-join `(,@(map (λ (x)(if (string? x) x (format "~v" x))) items)))))
