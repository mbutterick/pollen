#lang racket/base
(require racket/string racket/list racket/contract)
(require "hyphenation-data.rkt")
(require "../readability.rkt")
(require "../tools.rkt")

(module+ test (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hyphenate.rkt
;;; Racket port of Ned Batchelder's hyphenate.py
;;; http://nedbatchelder.com/code/modules/hyphenate.html
;;; (in the public domain)
;;; which in turn was an implementation
;;; of the Liang hyphenation algorithm in TeX
;;; (also in the public domain)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exceptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-exceptions exception-data) 
  (define (make-key x)
    (string-replace x "-" ""))
  
  (define (make-value x)
    (list->vector (cons 0 (map (λ(x) (if (equal? x "-") 1 0)) (regexp-split #px"[a-z]" x)))))
  
  (make-hash 
   (map (λ(x) (cons (make-key x) (make-value x))) exception-data)))

; global data, so this only needs to be defined once
(define exceptions (make-exceptions exception-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-pattern-tree pattern-data)
  (define tree (make-hash))
  (define (insert-pattern pat)
    (let* ([chars (regexp-replace* #px"[0-9]" pat "")]
           ;; regexp returns list of strings
           [points (map (λ(x) (if (> (len x) 0) (string->number x) 0)) (regexp-split #px"[.a-z]" pat))]
           [tree tree])
      (for ([char chars])
        (when (not (char . in? . tree))
          (change tree char (make-hash)))
        (set! tree (get tree char)))
      (change tree empty points)))
  (map insert-pattern pattern-data)
  tree)

; global data, so this only needs to be defined once
(define pattern-tree (make-pattern-tree pattern-data))

(define (make-points word) 
  
  (define (make-zeroes points)
    ; controls hyphenation zone from edges of word
    ; todo: parameterize this setting
    ; todo: does this count end-of-word punctuation? it shouldn't.
    (map (λ(i) (change points i 0)) (list 1 2 (- (len points) 2) (- (len points) 3)))
    points)
  
  (let* ([word (string-downcase word)]
         [points 
          (if (word . in? . exceptions)
              (get exceptions word)
              (let* ([work (string-append "." (->string word) ".")]
                     [points (make-vector (add1 (len work)) 0)]) 
                (for ([i (len work)])
                  (let ([tree pattern-tree])
                    (for ([char (get work i 'end)]
                          #:break (not (char . in? . tree)))
                      (set! tree (get tree char))
                      (when (empty . in? . tree)
                        (let ([point (get tree empty)])
                          (for ([j (len point)])
                            (change points (+ i j) (max (get points (+ i j)) (get point j)))))))))
                points))])
    
    ; make-zeroes controls minimum hyphenation distance from edge.
    ; todo: dropping first 2 elements is needed for mysterious reasons to be documented later
    ; see python code for why
    (get (make-zeroes points) 2 'end)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define/contract (word->hyphenated-pieces word #:omit [omit? (λ(x) #f)])
  ((string?) (#:omit procedure?) . ->* . (listof string?))
  ; Given a word, returns a list of pieces, 
  ; broken at the possible hyphenation points.
  (if (or (<= (len word) 4) (omit? word))
      ;; boundary conditions:
      ;; Short words aren't hyphenated, nor omitted words
      (->list word)  
      ; Examine the points to build the pieces list.
      (string-split ; split on whitespace
       (list->string ; concatenate chars
        (flatten ; get rid of cons pairs
         (for/list ([char word] 
                    [point (make-points word)])
           (if (even? point)
               char ; even point denotes character
               (cons char #\ )))))))) ; odd point denotes char + syllable


(define (hyphenate-string text #:joiner [joiner (integer->char #x00AD)] #:omit [omit? (λ(x)#f)])
  (regexp-replace* #px"\\w+" text (λ(word) (string-join (word->hyphenated-pieces word #:omit omit?) (->string joiner)))))

(define (capitalized? word)
  ; match property = \\p
  ; match unicode uppercase = {Lu}
  (regexp-match #px"\\p{Lu}" (get word 0))) 

(define (ligated? word)
  (ormap (λ(lig) (regexp-match lig word)) '("ff" "fi" "fl" "ffi" "ffl")))

(define (capitalized-or-ligated? word)
  ; filter function for hyphenate
  ; filtering ligatable words because once the soft hyphens go in,
  ; the browser won't automatically substitute the ligs.
  ; so it looks weird, because some are ligated and some not.
  ; not ideal, because it removes hyphenation options but ... whatever
  (or (capitalized? word) (ligated? word)))


(define (hyphenate x #:only [only-proc (λ(x) x)]) ; recursively hyphenate strings within xexpr
  (define exclusions '(style script)) ; omit these from ever being hyphenated
  
  (cond
    ; todo: the only-proc semantics are illogical.
    ; main issue: keep it out of tags like <style> that parse as textual elements, but are not.
    ; So two choices, opt-out or opt-in.
    ; Problem with opt-out: is set of outlier tags like <style> well-defined?
    ; Won't it make hyphenation naturally overinclusive?
    ; Problem with opt-in: conceals a lot of tags that naturally live inside other tags
    ; only reaches text at the "root level" of the tag.
    [(tagged-xexpr? x) (if (and (only-proc x) (not ((car x) . in? . exclusions)))
                           (map-xexpr-elements hyphenate x)
                           (map-xexpr-elements (λ(x) (if (tagged-xexpr? x) (hyphenate x) x)) x))] ; only process subxexprs
    
    [(string? x) (hyphenate-string x)]
    [else x]))

(module+ test
  (check-equal? (word->hyphenated-pieces "polymorphism") '("poly" "mor" "phism"))
  (check-equal? (hyphenate "circular polymorphism squandering") "cir\u00ADcu\u00ADlar poly\u00ADmor\u00ADphism squan\u00ADder\u00ADing")
  (check-equal? (hyphenate '(p "circular polymorphism")) '(p "cir\u00ADcu\u00ADlar poly\u00ADmor\u00ADphism")))