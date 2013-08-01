#lang racket/base
(require (only-in racket/list empty? range))
(require (only-in racket/format ~a ~v))
(require (only-in racket/string string-join)) 
(require (prefix-in williams: (planet williams/describe/describe)))
(require racket/date)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions for readability
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))

; lambda alias
; won't work as simple define because λ is specially handled in reader
(define-syntax-rule (ƒ x ...) (λ x ...))

(define (describe x)
  (williams:describe x)
  x)

; report the current value of the variable, then return it
(define-syntax-rule (report var)
  (begin 
    (message 'var "=" var) 
    var))

; debug utilities
(define (message . x)
  (define (zfill s n)
    (set! s (as-string s))
    (if (> (string-length s) n)
        s
        (string-append (make-string (- n (string-length s)) #\0) s)))
  
  (define (make-date-string)
    (define d (current-date))
    (define df (map (ƒ(x) (zfill x 2)) (list (date-month d)(date-day d)(date-year d)(modulo (date-hour d) 12)(date-minute d)(date-second d)(if (< (date-hour d) 12) "am" "pm"))))
    
    (apply format "[~a.~a.~a ~a:~a:~a~a]" df))
  (displayln (string-join `(,(make-date-string) ,@(map (ƒ(x)(if (string? x) x (~v x))) x))) (current-error-port)))



(define (exists? x)
  ; neither empty nor false
  (and (not (empty? x)) x))

      

#|(define (=str . xs)
  (let ([tester (car xs)])
    (all (ƒ(x) (equal? tester x)) (map as-string (cdr xs)))))|#

(define (=str . xs)
  (let* ([xs (map as-string xs)]
         [tester (car xs)])
    (all (ƒ(x) (equal? tester x)) (cdr xs))))

(define (int x)
  (cond
    [(integer? x) x]
    [(boolean? x) (if x 1 0)]
    [(real? x) (floor x)]
    [(string? x) (if (= (len x) 1)
                     (int (car (string->list x))) ; treat as char
                     (int (string->number x)))]
    [(symbol? x) (int (as-string x))]
    [(char? x) (char->integer x)]
    [(empty? x) 0]
    [(or (list? x) (hash? x) (vector? x)) (len x)]
    [else (error "Can't convert to integer:" x)]))

(define (str . x)
  (string-join (map as-string x) ""))

(define (len x)
  (cond
    [(list? x) (length x)]
    [(string? x) (string-length x)]
    [(symbol? x) (len (as-string x))]
    [(vector? x) (vector-length x)]
    [(hash? x) (len (hash-keys x))]
    [else #f]))

(define (change x i value)
  ; general-purpose mutable data object setter
  (cond
    [(vector? x) (vector-set! x i value)] 
    [(hash? x) (hash-set! x i value)]
    [else (error "Can't set this datatype using !")]))

(define (get x i [j #f])
  (when (and (or (list? x) (string? x) (vector? x)) j)
    (cond 
      [(and (real? j) (< j 0)) (set! j (+ (len x) j))]
      [(equal? j 'end) (set! j (len x))]))
  
  (cond
    [(list? x) (if j
                   (for/list ([index (range i j)])
                     (get x index))
                   (list-ref x i))]
    [(vector? x) (if j
                     (for/vector ([index (range i j)])
                       (get x index))
                     (vector-ref x i))]
    [(string? x) (if j
                     (substring x i j)
                     (get x i (add1 i)))]
    [(symbol? x) (as-symbol (get (as-string x) i j))]
    [(hash? x) (if j
                   (error "get: third arg not supported for hash")
                   (hash-ref x i))]
    [else #f]))

(define (in? container element)
  (cond
    [(list? container) (member element container)]
    [(hash? container) (hash-has-key? container element)]
    ; todo: should this handle arbitrary-length substrings?
    ; leaning toward no, because it breaks the string-as-array-of-characters abstraction
    [(string? container) (let ([result (in? (map as-string (string->list container)) (as-string element))])
                   (if result
                       (string-join result "")
                       #f))]
    [(symbol? container) (let ([result (in? (as-string container) element)])
                   (if result
                       (as-symbol result)
                       result))]
    [else #f]))

(define (to-lc x)
  (string-downcase x))

(define (to-uc x)
  (string-upcase x))

; python-style string testers
(define (starts-with? string starter)
  (if (<= (len starter) (len string))
      (equal? (get string 0 (len starter)) starter)
      #f))

(define (ends-with? string ender)
  (if (<= (len ender) (len string) )
      (equal? (get string (- (len string) (len ender)) 'end) ender)
      #f))

; coercions
(define (as-path thing)
  (set! thing 
        (if (string? thing) 
            (string->path thing)
            thing))
  (when (not (path? thing)) (error (format "Can't make ~a into path" thing)))
  thing)

(define (as-list thing)
  (set! thing 
        (if (not (list? thing)) 
            (list thing)
            thing))
  (when (not (list? thing)) (error (format "Can't make ~a into list" thing)))
  thing) 

; nice way of converting to string
(define (as-string x)
  (set! x (cond 
            [(empty? x) ""]
            [(symbol? x) (symbol->string x)]
            [(number? x) (number->string x)]
            [(path? x) (path->string x)]
            [(char? x) (~a x)]
            [else x]))
  (when (not (string? x)) (error (format "Can't make ~a into string" x)))
  x)

; nice way of converting to symbol
; todo: on bad input, it will pop a string error rather than symbol error
(define (as-symbol thing)
  (string->symbol (as-string thing))) 

; nice way of converting to path
(define (as-complete-path thing)
  (path->complete-path (as-path thing)))

; any & all & none
(define (any tests things)
  (ormap (ƒ(test) (ormap test (as-list things))) (as-list tests)))

(define (all tests things)
  (andmap (ƒ(test) (andmap test (as-list things))) (as-list tests)))

(define (none test things) (not (any test things)))


; Other possibilities
; trim
; split