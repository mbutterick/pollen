#lang racket/base
(require racket/contract net/url)
(require (only-in racket/list empty? range splitf-at dropf dropf-right))
(require (only-in racket/format ~a))
(require (only-in racket/string string-join))
(require (only-in racket/vector vector-member))
(require (only-in racket/set set set->list set?))
(module+ test (require rackunit))
(require "debug.rkt")

(provide (all-defined-out))

;; general way of coercing to integer
(define/contract (->int x)
  (any/c . -> . integer?)
  (cond
    [(integer? x) x]
    [(real? x) (floor x)]
    [(and (string? x) (> (len x) 0)) (->int (string->number x))]
    [(symbol? x) (->int (->string x))]
    [(char? x) (char->integer x)]
    [(has-length? x) (len x)]
    [else (error "Can't convert to integer:" x)]))


;; general way of coercing to string
(define/contract (->string x)
  (any/c . -> . string?)
  (cond 
    [(string? x) x]
    [(empty? x) ""]
    [(symbol? x) (symbol->string x)]
    [(number? x) (number->string x)]
    [(url? x) (->string (->path x))]
    [(path? x) (path->string x)]
    [(char? x) (~a x)]
    [else (error (format "Can't make ~a into string" x))]))

(module+ test
  (check-equal? (->string "foo") "foo")
  (check-equal? (->string '()) "")
  (check-equal? (->string 'foo) "foo")
  (check-equal? (->string 123) "123")
  (check-equal? (->string (string->url "foo/bar.html")) "foo/bar.html")
  (define file-name-as-text "foo.txt")
  (check-equal? (->string (string->path file-name-as-text)) file-name-as-text)
  (check-equal? (->string #\¶) "¶"))


;; general way of coercing to symbol
(define (->symbol thing)
  ; todo: on bad input, it will pop a string error rather than symbol error
  (string->symbol (->string thing))) 

;; general way of coercing to path
(define/contract (->path thing)
  (any/c . -> . path?)
  ; todo: on bad input, it will pop a string error rather than path error
  (cond 
    [(url? thing) (apply build-path (map path/param-path (url-path thing)))]
    [else (string->path (->string thing))]))

(module+ test
  (check-equal? (->path "foo") (string->path "foo"))
  (check-equal? (->path 'foo) (string->path "foo"))
  (check-equal? (->path 123) (string->path "123"))
  (check-equal? (->path (string->url "foo/bar.html")) (string->path "foo/bar.html")))

;; general way of coercing to url
(define/contract (->url thing)
  (any/c . -> . url?)
  ; todo: on bad input, it will pop a string error rather than url error
  (string->url (->string thing)))

(define (->complete-path thing)
  (path->complete-path (->path thing)))


;; general way of coercing to a list
(define/contract (->list x)
  (any/c . -> . list?)
  (cond 
    [(list? x) x]
    [(vector? x) (vector->list x)]
    [(set? x) (set->list x)]
    [else (list x)])) 

(module+ test
  (check-equal? (->list '(1 2 3)) '(1 2 3))
  (check-equal? (->list (list->vector '(1 2 3))) '(1 2 3))
  (check-equal? (->list (set 1 2 3)) '(3 2 1))
  (check-equal? (->list "foo") (list "foo")))

;; general way of coercing to vector
(define (->vector x)
  (any/c . -> . vector?)
  ; todo: on bad input, it will pop a list error rather than vector error
  (cond
    [(vector? x) x]
    [else (list->vector (->list x))]))



;; general way of coercing to boolean
(define/contract (->boolean x)
  (any/c . -> . boolean?)
  ;; in Racket, everything but #f is true
  (if x #t #f))

(module+ test
  (check-true (->boolean #t))
  (check-false (->boolean #f))
  (check-true (->boolean "#f")) 
  (check-true (->boolean "foo"))
  (check-true (->boolean '()))
  (check-true (->boolean '(1 2 3))))


(define/contract (has-length? x)
  (any/c . -> . boolean?)
  (ormap (λ(proc) (proc x)) (list list? string? symbol? vector? hash? set?)))

;; general way of asking for length
(define/contract (len x)
  (has-length? . -> . integer?)
  (cond
    [(list? x) (length x)]
    [(string? x) (string-length x)]
    [(symbol? x) (len (->string x))]
    [(vector? x) (len (->list x))]
    [(hash? x) (len (hash-keys x))]
    [(set? x) (len (->list x))]
    [else #f]))

(module+ test
  (check-equal? (len '(1 2 3)) 3)
  (check-not-equal? (len '(1 2)) 3) ; len 2
  (check-equal? (len "foo") 3)
  (check-not-equal? (len "fo") 3) ; len 2
  (check-equal? (len 'foo) 3)
  (check-not-equal? (len 'fo) 3) ; len 2
  (check-equal? (len (list->vector '(1 2 3))) 3)
  (check-not-equal? (len (list->vector '(1 2))) 3) ; len 2
  (check-equal? (len (set 1 2 3)) 3)
  (check-not-equal? (len (set 1 2)) 3) ; len 2
  (check-equal? (len (make-hash '((a . 1) (b . 2) (c . 3)))) 3)
  (check-not-equal? (len (make-hash '((a . 1) (b . 2)))) 3)) ; len 2



(define/contract (sliceable-container? x)
  (any/c . -> . boolean?)
  (ormap (λ(proc) (proc x)) (list list? string? symbol? vector?)))

(define/contract (gettable-container? x)
  (any/c . -> . boolean?)
  (ormap (λ(proc) (proc x)) (list sliceable-container? hash?))) 


;; general way of setting an item in a mutable container
(define/contract (change x i value)
  ((or/c vector? hash?) any/c any/c . -> . void?)
  ; general-purpose mutable data object setter
  (cond
    [(vector? x) (vector-set! x i value)] 
    [(hash? x) (hash-set! x i value)]
    [else (error "Can't set this datatype using change")]))


;; general way of fetching an item from a container
(define/contract (get container start [end #f])
  ((gettable-container? any/c) ((λ(i)(or (integer? i) (and (symbol? i) (equal? i 'end))))) 
                               . ->* . any/c)
  
  (set! end
        (if (sliceable-container? container)
            (cond 
              ;; treat negative lengths as offset from end (Python style)
              [(and (integer? end) (< end 0)) (+ (len container) end)]
              ;; 'end slices to the end
              [(equal? end 'end) (len container)]
              ;; default to slice length of 1 (i.e, single-item retrieval)
              [(equal? end #f) (add1 start)]
              [else end])
            end))
  
  (define result (cond
                   ;; for sliceable containers, make a slice
                   [(list? container) (for/list ([i (range start end)]) 
                                        (list-ref container i))]
                   [(vector? container) (for/vector ([i (range start end)])
                                          (vector-ref container i))] 
                   [(string? container) (substring container start end)]
                   [(symbol? container) (->symbol (get (->string container) start end))] 
                   ;; for hash, just get item
                   [(hash? container) (hash-ref container start)]
                   [else #f]))
  
  ;; don't return single-item results inside a list
  (if (and (sliceable-container? container) (= (len result) 1))
      (car (->list result))
      result))

(module+ test
  (check-equal? (get '(0 1 2 3 4 5) 2) 2)
  (check-equal? (get `(0 1 ,(list 2) 3 4 5) 2) (list 2))
  (check-equal? (get '(0 1 2 3 4 5) 0 2) '(0 1))
  (check-equal? (get '(0 1 2 3 4 5) 2 -1) '(2 3 4))
  (check-equal? (get '(0 1 2 3 4 5) 2 'end) '(2 3 4 5))
  (check-equal? (get (list->vector '(0 1 2 3 4 5)) 2) 2)
  (check-equal? (get (list->vector'(0 1 2 3 4 5)) 0 2) (list->vector '(0 1)))
  (check-equal? (get (list->vector'(0 1 2 3 4 5)) 2 -1) (list->vector '(2 3 4)))
  (check-equal? (get (list->vector'(0 1 2 3 4 5)) 2 'end) (list->vector '(2 3 4 5)))
  (check-equal? (get "purple" 2) "r")
  (check-equal? (get "purple" 0 2) "pu")
  (check-equal? (get "purple" 2 -1) "rpl")
  (check-equal? (get "purple" 2 'end) "rple")
  (check-equal? (get 'purple 2) 'r)
  (check-equal? (get 'purple 0 2) 'pu)
  (check-equal? (get 'purple 2 -1) 'rpl)
  (check-equal? (get 'purple 2 'end) 'rple)
  (check-equal? (get (make-hash `((a . ,(list 1)) (b . ,(list 2)) (c  . ,(list 3)))) 'a) (list 1)))

;; general way of testing for membership (à la Python 'in')
;; put item as first arg so function can use infix notation
;; (item . in . container)
(define/contract (in? item container)
  (any/c any/c . -> . boolean?)
  (->boolean (cond
               [(list? container) (member item container)] ; returns #f or sublist beginning with item
               [(vector? container) (vector-member item container)] ; returns #f or zero-based item index
               [(hash? container) 
                (and (hash-has-key? container item) (get container item))] ; returns #f or hash value
               [(string? container) ((->string item) . in? . (map ->string (string->list container)))] ; returns #f or substring beginning with item
               [(symbol? container) ((->string item) . in? . (->string container))] ; returns #f or subsymbol (?!) beginning with item
               [else #f])))

(module+ test
  (check-true (2 . in? . '(1 2 3)))
  (check-false (4 . in? . '(1 2 3)))
  (check-true (2 . in? . (list->vector '(1 2 3))))
  (check-false (4 . in? . (list->vector '(1 2 3))))
  (check-true ('a . in? . (make-hash '((a . 1) (b . 2) (c  . 3)))))
  (check-false ('x . in? . (make-hash '((a . 1) (b . 2) (c  . 3)))))
  (check-true ("o" . in? . "foobar"))
  (check-false ("z" . in? . "foobar"))
  (check-true ('o . in? . 'foobar))
  (check-false ('z . in? . 'foobar))
  (check-false ("F" . in? . #\F)))


;; stringish: data type that can be trivially converted to string
;; todo: merge this with pathish
(define/contract (stringish? x)
  (any/c . -> . boolean?)
  (with-handlers ([exn:fail? (λ(e) #f)])
    (->boolean (->string x))))

;; python-style string testers
(define/contract (starts-with? str starter)
  (stringish? stringish? . -> . boolean?)
  (let ([str (->string str)]
        [starter (->string starter)])
    (and (<= (len starter) (len str)) (equal? (get str 0 (len starter)) starter))))

(module+ test
  (check-true ("foobar" . starts-with? . "foo"))
  (check-true ("foobar" . starts-with? . "f"))
  (check-true ("foobar" . starts-with? . "foobar"))
  (check-false ("foobar" . starts-with? . "bar")))

(define/contract (ends-with? str ender)
  (string? string? . -> . boolean?)
  (and (<= (len ender) (len str)) (equal? (get str (- (len str) (len ender)) 'end) ender)))

(module+ test
  (check-true ("foobar" . ends-with? . "bar"))
  (check-true ("foobar" . ends-with? . "r"))
  (check-true ("foobar" . ends-with? . "foobar"))
  (check-false ("foobar" . ends-with? . "foo")))

;; trim from beginning & end of list
(define (trim items test-proc)
  (list? procedure? . -> . list?)
  (dropf-right (dropf items test-proc) test-proc))

(module+ test
  ;  (check-equal? (trim (list "\n" " " 1 2 3 "\n") whitespace?) '(1 2 3))
  (check-equal? (trim (list 1 3 2 4 5 6 8 9 13) odd?) '(2 4 5 6 8)))



;; split list into list of sublists using test-proc
(define/contract (splitf-at* xs split-test)
  ;; todo: better error message when split-test is not a predicate
  (list? predicate/c . -> . (listof list?))
  (define (&splitf-at* xs [acc '()]) ; use acc for tail recursion
    (if (empty? xs) 
        ;; reverse because accumulation is happening backward 
        ;; (because I'm using cons to push latest match onto front of list)
        (reverse acc)
        (let-values ([(item rest) 
                      ;; drop matching elements from front
                      ;; then split on nonmatching 
                      ;; = nonmatching item + other elements (which will start with matching)
                      (splitf-at (dropf xs split-test) (compose1 not split-test))])
          ;; recurse, and store new item in accumulator
          (&splitf-at* rest (cons item acc)))))
  
  ;; trim off elements matching split-test
  (&splitf-at* (trim xs split-test)))

(module+ test
  ; (check-equal? (splitf-at* '("foo" " " "bar" "\n" "\n" "ino") whitespace?) '(("foo")("bar")("ino")))
  (check-equal? (splitf-at* '(1 2 3 4 5 6) even?) '((1)(3)(5))))


;; convert a bytecount into a string
(define/contract (bytecount->string bytecount)
  (integer? . -> . string?)
  (define (format-with-threshold threshold suffix)
    ;; upconvert by factor of 100 to get two digits after decimal
    (format "~a ~a" (exact->inexact (/ (round ((* bytecount 100) . / . threshold)) 100)) suffix))
  
  (define threshold-kilobyte 1000)
  (define threshold-megabyte (threshold-kilobyte . * . threshold-kilobyte))
  (define threshold-gigabyte (threshold-megabyte . * . threshold-kilobyte))
  (define threshold-terabyte (threshold-gigabyte . * . threshold-kilobyte))
  
  (cond
    [(bytecount . >= . threshold-terabyte) (format-with-threshold threshold-terabyte "TB")]
    [(bytecount . >= . threshold-gigabyte) (format-with-threshold threshold-gigabyte "GB")]
    [(bytecount . >= . threshold-megabyte) (format-with-threshold threshold-megabyte "MB")]
    [(bytecount . >= . threshold-kilobyte) (format-with-threshold threshold-kilobyte "KB")]
    [else (format "~a bytes" bytecount)]))


;; for use inside quasiquote
;; instead of ,(when ...) use ,@(when/splice ...)
;; to avoid voids
(define-syntax-rule (when/splice test body) (if test (list body) '()))
