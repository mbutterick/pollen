#lang racket/base
(require racket/string racket/list)
(require (planet mb/pollen/hyphenation-data))
(require (planet mb/pollen/readability))
(require (planet mb/pollen/tools))

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
    (list->vector (cons 0 (map (ƒ(x) (int (=str x "-"))) (regexp-split #px"[a-z]" x)))))
  
  (make-hash 
   (map (ƒ(x) (cons (make-key x) (make-value x))) exception-data)))

; global data, so this only needs to be defined once
(define exceptions (make-exceptions exception-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-pattern-tree pattern-data)
  (define tree (make-hash))
  (define (insert-pattern pat)
    (let* ([chars (regexp-replace* #px"[0-9]" pat "")]
           [points (map (λ(x) (int x)) (regexp-split #px"[.a-z]" pat))]
           [tree tree])
      (for ([char chars])
        (when (not (in? tree char))
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
    (map (ƒ(i) (change points i 0)) (list 1 2 (- (len points) 2) (- (len points) 3)))
    points)
  
  (let* ([word (to-lc word)]
         [points 
          (if (in? exceptions word)
              (get exceptions word)
              (let* ([work (str "." word ".")]
                     [points (make-vector (add1 (len work)) 0)]) 
                (for ([i (len work)])
                  (let ([tree pattern-tree])
                    (for ([char (get work i 'end)]
                          #:break (not (in? tree char)))
                      (set! tree (get tree char))
                      (when (in? tree empty)
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


(define (hyphenate-word word #:filter [filter (λ(x)x)])
  ; Given a word, returns a list of pieces, 
  ; broken at the possible hyphenation points.
  (if (or (<= (len word) 4) (filter word))
      ; Short words aren't hyphenated.
      (as-list word)  
      ; Examine the points to build the pieces list.
      (string-split ; split on whitespace
       (list->string ; concatenate chars
        (flatten ; get rid of cons pairs
         (for/list ([char word] [point (make-points word)])
           (if (even? point)
               char ; even point denotes character
               (cons char #\ )))))))) ; odd point denotes char + syllable


(define (hyphenate-string text #:joiner [joiner (integer->char #x00AD)] #:filter [filter (λ(x)x)])
  (regexp-replace* #px"\\w+" text (ƒ(word) (string-join (hyphenate-word word #:filter filter) (as-string joiner)))))

(define (capitalized? word)
  ; match property = \\p
  ; match unicode uppercase = {Lu}
  (regexp-match #px"\\p{Lu}" (get word 0))) 


(define (hyphenate x #:only [only-proc (ƒ(x) x)]) ; recursively hyphenate strings within xexpr
  (define exclusions '(style script)) ; omit these from ever being hyphenated
  (define (capitalized-or-ligated? word)
    ; filter function for hyphenate
    ; filtering ligatable words because once the soft hyphens go in,
    ; the browser won't automatically substitute the ligs.
    ; so it looks weird, because some are ligated and some not.
    ; not ideal, because it removes hyphenation options but ... whatever
    (or (capitalized? word) (any (ƒ(lig) (regexp-match lig word)) '("ff" "fi" "fl" "ffi" "ffl"))))
  
  (cond
    ; todo: the only-proc semantics are illogical.
    ; main issue: keep it out of tags like <style> that parse as textual elements, but are not.
    ; So two choices, opt-out or opt-in.
    ; Problem with opt-out: is set of outlier tags like <style> well-defined?
    ; Won't it make hyphenation naturally overinclusive?
    ; Problem with opt-in: conceals a lot of tags that naturally live inside other tags
    ; only reaches text at the "root level" of the tag.
    [(named-xexpr? x) (if (and (only-proc x) (not (in? exclusions (car x))))
                          (map-xexpr-content hyphenate x)
                          (map-xexpr-content hyphenate x #:only named-xexpr?))] ; only process subxexprs
    
    [(string? x) 
     ; hyphenate everything but last word
     ; todo: problem here is that it's string-based, not paragraph based.
     ; meaning, the last word of every STRING gets exempted, 
     ; even if that word doesn't fall at the end of a block. 
     ; should work the way nonbreak spacer works.
     ; todo: question - should hyphenator ignore possible ligature pairs, like fi?
     ; because auto ligatures will skip combos with a soft hyphen between
     
     
     ; regexp matches everything up to last word, and allows trailing whitespace
     ; parenthesized matches become series of lambda arguments. Arity must match
     ; [^\\s\u00A0] = characters that are neither whitespace nor nbsp (which is not included in \s)
     ; +\\s*$ = catches trailing whitespace up to end 
     (regexp-replace #px"(.*?)([^\\s\u00A0]+\\s*$)" 
                     x 
                     ; by default, filter out capitalized words and words with ligatable combos
                     ; m0 m1 m2 are the match groups from regexp-replace
                     (ƒ(m0 m1 m2) (string-append (hyphenate-string m1 #:filter capitalized-or-ligated?) m2)))]
    [else x]))

(module+ main
  (hyphenate '(p "circular firing squad") #:only (ƒ(xexpr) (in? '(p) (first xexpr)))))