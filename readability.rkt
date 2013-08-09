#lang racket/base
(require racket/contract)
(require (only-in racket/list empty? range))
(require (only-in racket/format ~a))
(require (only-in racket/string string-join))
(require (only-in racket/vector vector-member))
(module+ test (require rackunit))
(require "debug.rkt")

(provide (all-defined-out))

;; general way of coercing to string
(define/contract (->string x)
  (any/c . -> . string?)
  (cond 
    [(string? x) x]
    [(empty? x) ""]
    [(symbol? x) (symbol->string x)]
    [(number? x) (number->string x)]
    [(path? x) (path->string x)]
    [(char? x) (~a x)]
    [else (error (format "Can't make ~a into string" x))]))

(module+ test
  (check-equal? (->string "foo") "foo")
  (check-equal? (->string '()) "")
  (check-equal? (->string 'foo) "foo")
  (check-equal? (->string 123) "123")
  (define file-name-as-text "foo.txt")
  (check-equal? (->string (string->path file-name-as-text)) file-name-as-text)
  (check-equal? (->string #\¶) "¶"))


;; general way of coercing to symbol
(define (->symbol thing)
  ; todo: on bad input, it will pop a string error rather than symbol error
  (string->symbol (->string thing))) 


;; general way of coercing to a list
(define (->list x)
  (any/c . -> . list?)
  (cond 
    [(list? x) x]
    [(vector? x) (vector->list x)]
    [else (list x)])) 

(module+ test
  (check-equal? (->list '(1 2 3)) '(1 2 3))
  (check-equal? (->list (list->vector '(1 2 3))) '(1 2 3))
  (check-equal? (->list "foo") (list "foo")))


;; general way of coercing to boolean
(define (->boolean x)
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



;; general way of asking for length
(define (len x)
  (any/c . -> . integer?)
  (cond
    [(list? x) (length x)]
    [(string? x) (string-length x)]
    [(symbol? x) (len (->string x))]
    [(vector? x) (vector-length x)]
    [(hash? x) (len (hash-keys x))]
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
  (check-equal? (len (make-hash '((a . 1) (b . 2) (c . 3)))) 3)
  (check-not-equal? (len (make-hash '((a . 1) (b . 2)))) 3)) ; len 2



;; general way of fetching an item from a container
(define/contract (get container start [end #f])
  ((any/c any/c) ((λ(i)(or (integer? i) (and (symbol? i) (equal? i 'end))))) 
                 . ->* . any/c)
  
  (define (sliceable-container? container)
    (ormap (λ(proc) (proc container)) (list list? string? vector?)))
  
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
                   [(hash? container) (let ([hash-key start])
                                        (hash-ref container hash-key))]
                   [else #f]))
  
  ;; don't return single-item results inside a list
  (if (and (sliceable-container? result) (= (len result) 1))
      (car (->list result))
      result))

(module+ test
  (check-equal? (get '(0 1 2 3 4 5) 2) 2)
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
  (check-equal? (get (make-hash '((a . 1) (b . 2) (c  . 3))) 'a) 1))

;; general way of testing for membership (à la Python 'in')
;; put item as first arg so function can use infix notation
;; (item . in . container)
(define/contract (in item container)
  (any/c any/c . -> . any/c)
  (cond
    [(list? container) (member item container)] ; returns #f or sublist beginning with item
    [(vector? container) (vector-member item container)] ; returns #f or zero-based item index
    [(hash? container) 
     (and (hash-has-key? container item) (get container item))] ; returns #f or hash value
    [(string? container) (let ([result ((->string item) . in . (map ->string (string->list container)))])
                           (if result
                               (string-join result "")
                               #f))] ; returns #f or substring beginning with item
    [(symbol? container) (let ([result ((->string item) . in . (->string container))])
                           (if result
                               (->symbol result)
                               result))] ; returns #f or subsymbol (?!) beginning with item
    [else #f]))

(module+ test
  (check-equal? (2 . in . '(1 2 3)) '(2 3))
  (check-false (4 . in . '(1 2 3)))
  (check-equal? (2 . in . (list->vector '(1 2 3))) 1)
  (check-false (4 . in . (list->vector '(1 2 3))))
  (check-equal? ('a . in . (make-hash '((a . 1) (b . 2) (c  . 3)))) 1)
  (check-false ('x . in . (make-hash '((a . 1) (b . 2) (c  . 3)))))
  (check-equal? ("o" . in . "foobar") "oobar")
  (check-false ("z" . in . "foobar"))
  (check-equal? ('o . in . 'foobar) 'oobar)
  (check-false ('z . in . 'foobar))
  (check-false ("F" . in . #\F)))