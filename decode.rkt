#lang racket/base
(require racket/contract racket/list racket/string racket/match)
(require (only-in xml xexpr/c))
(require "tools.rkt" "predicates.rkt" sugar txexpr)

(module+ test (require rackunit))

(provide (except-out (all-defined-out) decode register-block-tag))


;; add a block tag to the list
;; this function is among the predicates because it alters a predicate globally.
(define+provide/contract (register-block-tag tag)
  (symbol? . -> . void?)
  (append-block-tag tag))

(module+ test
  (check-true (begin (register-block-tag 'barfoo) (block-xexpr? '(barfoo "foo")))))


;; decoder wireframe
(define+provide/contract (decode nx
                #:exclude-xexpr-tags [excluded-xexpr-tags '()]
                #:xexpr-tag-proc [xexpr-tag-proc (λ(x)x)]
                #:xexpr-attrs-proc [xexpr-attrs-proc (λ(x)x)]
                #:xexpr-elements-proc [xexpr-elements-proc (λ(x)x)]
                #:block-xexpr-proc [block-xexpr-proc (λ(x)x)]
                #:inline-xexpr-proc [inline-xexpr-proc (λ(x)x)]
                #:string-proc [string-proc (λ(x)x)])
  ((xexpr/c) ;; use xexpr/c for contract on nx because it gives better error messages
                   
                   ;; todo: how to write more specific contracts for these procedures?
                   ;; e.g., string-proc should be restricted to procs that accept a string as input
                   ;; and return a string as output
                   (#:exclude-xexpr-tags list?
                                         #:xexpr-tag-proc procedure?
                                         #:xexpr-attrs-proc procedure?
                                         #:xexpr-elements-proc procedure?
                                         #:block-xexpr-proc procedure?
                                         #:inline-xexpr-proc procedure?
                                         #:string-proc procedure?)
                   . ->* . txexpr?)
  (when (not (txexpr? nx))
    (error (format "decode: ~v not a full txexpr" nx)))
  
  
  (define (&decode x)
    (cond
      [(txexpr? x) (let-values([(tag attr elements) (txexpr->values x)]) 
                           (if (tag . in? . excluded-xexpr-tags)    
                               x ; let x pass through untouched
                               (let ([decoded-xexpr (apply make-txexpr 
                                                           (map &decode (list tag attr elements)))])
                                 ((if (block-xexpr? decoded-xexpr)
                                      block-xexpr-proc
                                      inline-xexpr-proc) decoded-xexpr))))]
      [(txexpr-tag? x) (xexpr-tag-proc x)]
      [(txexpr-attrs? x) (xexpr-attrs-proc x)]
      ;; need this for operations that may depend on context in list
      [(txexpr-elements? x) (map &decode (xexpr-elements-proc x))]
      [(string? x) (string-proc x)]
      ;; if something has made it through undecoded, that's a problem
      [else (error "Can't decode" x)]))
  
  
  (&decode nx))


;; This module is a library of functions to be used in building pollen decoders.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Typography 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; insert typographic niceties
;; ligatures are handled in css
(define (typogrify str)
  (string? . -> . string?)
  ;; make set of functions for replacers
  (define (make-replacer query replacement)
    (λ(str) (regexp-replace* query str replacement)))
  
  ;; just store the query strings + replacement strings
  (define dashes 
    ;; fix em dashes first, else they'll be mistaken for en dashes
    ;; [\\s ] is whitespace + nonbreaking space
    '((#px"[\\s ]*(---|—)[\\s ]*" "—") ; em dash
      (#px"[\\s ]*(--|–)[\\s ]*" "–"))) ; en dash
  
  (define smart-quotes
    '((#px"(?<=\\w)'(?=\\w)" "’") ; apostrophe
      (#px"(?<!\\w)'(?=\\w)" "‘") ; single_at_beginning
      (#px"(?<=\\S)'(?!\\w)" "’") ; single_at_end
      (#px"(?<!\\w)\"(?=\\w)" "“") ; double_at_beginning
      (#px"(?<=\\S)\"(?!\\w)" "”"))) ; double_at_end
  
  
  ;; put replacers in desired order here
  (let* ([typogrifiers (append dashes smart-quotes)]
         [queries (map first typogrifiers)]
         [replacements (map second typogrifiers)])
    (define replacers (map make-replacer queries replacements)) 
    ;; compose goes from last to first, so reverse order
    ((apply compose1 (reverse replacers)) str)))


(module+ test
  (check-equal? (typogrify "I had --- maybe 13 -- 20 --- hob-nobs.") "I had—maybe 13–20—hob-nobs.")
  (check-equal? (typogrify "\"Why,\" she could've asked, \"are we in O‘ahu watching 'Mame'?\"") 
                "“Why,” she could’ve asked, “are we in O‘ahu watching ‘Mame’?”"))

;; insert nbsp between last two words
(define/contract (nonbreaking-last-space x 
                                         #:nbsp [nbsp (->string #\u00A0)] 
                                         #:minimum-word-length [minimum-word-length 6])
  ((txexpr?) (#:nbsp string? #:minimum-word-length integer?) . ->* . txexpr?)
  ;; todo: parameterize this, as it will be different for each project
  (define tags-to-pay-attention-to '(p aside)) ; only apply to paragraphs
  
  (define (replace-last-space str)
    (if (#\space . in? . str)
        (let ([reversed-str-list (reverse (string->list str))]
              [reversed-nbsp (reverse (string->list nbsp))])
          (define-values (last-word-chars other-chars) 
            (splitf-at reversed-str-list (λ(i) (not (eq? i #\space)))))
          (list->string (reverse (append last-word-chars 
                                         ; OK for long words to be on their own line.
                                         (if (< (len last-word-chars) minimum-word-length)
                                             ; first char of other-chars will be the space, so use cdr
                                             (append reversed-nbsp (cdr other-chars))
                                             other-chars)))))
        str))
  
  (define (find-last-word-space x) ; recursively traverse xexpr
    (cond
      [(string? x) (replace-last-space x)] 
      [(txexpr? x) 
       (let-values([(tag attr elements) (txexpr->values x)])
         (if (> (length elements) 0) ; elements is list of xexprs
             (let-values ([(all-but-last last) (split-at elements (sub1 (length elements)))]) 
               (make-txexpr tag attr `(,@all-but-last ,(find-last-word-space (car last)))))
             x))]
      [else x]))
  
  (if ((car x) . in? . tags-to-pay-attention-to)
      (find-last-word-space x)
      x))

;; todo: make some tougher tests, it gets flaky with edge cases
(module+ test
  (check-equal? (nonbreaking-last-space '(p "Hi there")) '(p "Hi there")) ; nbsp in between last two words
  (check-equal? (nonbreaking-last-space '(p "Hi there") #:nbsp "Ø") '(p "HiØthere")) ; but let's make it visible
  (check-equal? (nonbreaking-last-space '(p "Hi there") #:nbsp "_up_") '(p "Hi_up_there")) 
  (check-equal? (nonbreaking-last-space '(p "Hi there") #:nbsp "_up_" #:minimum-word-length 3) 
                '(p "Hi there")) 
  (check-equal? (nonbreaking-last-space '(p "Hi here" (em "ho there")) #:nbsp "Ø") '(p "Hi here" (em "hoØthere")))) 


; wrap initial quotes for hanging punctuation
; todo: improve this
; does not handle <p>“<em>thing</em> properly
(define/contract (wrap-hanging-quotes nx 
                                      #:single-prepend [single-pp '(squo)]
                                      #:double-prepend  [double-pp '(dquo)])
  ((txexpr?) (#:single-prepend list? #:double-prepend list?) . ->* . txexpr?)
  (define two-or-more-char-string? (λ(i) (and (string? i) (>= (len i) 2))))
  (define-values (tag attr elements) (txexpr->values nx))
  (make-txexpr tag attr
                     (if (and (list? elements) (not (empty? elements)))
                         (let ([new-car-elements  (match (car elements)
                                                    [(? two-or-more-char-string? tcs)
                                                     (define str-first (get tcs 0))
                                                     (define str-rest (get tcs 1 'end))
                                                     (cond
                                                       [(str-first . in? . '("\"" "“"))
                                                        ;; can wrap with any inline tag
                                                        ;; so that linebreak detection etc still works
                                                        `(,@double-pp ,(->string #\“) ,str-rest)]
                                                       [(str-first . in? . '("\'" "‘")) 
                                                        `(,@single-pp ,(->string #\‘) ,str-rest)]
                                                       [else tcs])]
                                                    [(? txexpr? nx) (wrap-hanging-quotes nx)]
                                                    [else (car elements)])])
                           (cons new-car-elements (cdr elements)))
                         elements)))



(module+ test
  (check-equal? (wrap-hanging-quotes '(p "\"Hi\" there")) '(p (dquo "“" "Hi\" there")))
  (check-equal? (wrap-hanging-quotes '(p "'Hi' there")) '(p (squo "‘" "Hi' there")))
  (check-equal? (wrap-hanging-quotes '(p "'Hi' there") #:single-prepend '(foo ((bar "ino")))) 
                '(p (foo ((bar "ino")) "‘" "Hi' there")))
  ;; make sure txexpr without elements passes through unscathed
  (check-equal? (wrap-hanging-quotes '(div ((style "height:2em")))) '(div ((style "height:2em")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lines, blocks, paragraphs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; turn the right items into <br> tags
(define/contract (convert-linebreaks xc #:newline [newline "\n"])
  ((txexpr-elements?) (#:newline string?) . ->* . txexpr-elements?)
  ;; todo: should this test be not block + not whitespace?
  (define not-block? (λ(i) (not (block-xexpr? i))))
  (filter-not empty?
              (for/list ([i (len xc)])
                (let ([item (get xc i)])
                  (cond
                    ;; skip first and last
                    [(or (= i 0) (= i (sub1 (len xc)))) item]
                    [(equal? item newline)                   
                     (match (get xc (- i 1) (+ i 2)) ; a three-element slice with x[i] in the middle
                       ;; only convert if neither adjacent tag is a block
                       ;; (because blocks automatically force a newline before & after)
                       [(list (? not-block?) newline (? not-block?)) '(br)]
                       [else empty])] ; otherwise delete
                    [else item])))))

(module+ test
  (check-equal? (convert-linebreaks '("foo" "\n" "bar")) '("foo" (br) "bar"))
  (check-equal? (convert-linebreaks '("\n" "foo" "\n" "bar" "\n")) '("\n" "foo" (br) "bar" "\n"))
  (check-equal? (convert-linebreaks '((p "foo") "\n" (p "bar"))) '((p "foo") (p "bar")))
  (check-equal? (convert-linebreaks '("foo" "\n" (p "bar"))) '("foo" (p "bar")))
  (check-equal? (convert-linebreaks '("foo" "moo" "bar")) '("foo" "moo" "bar"))
  (check-equal? (convert-linebreaks '("foo" "moo" "bar") #:newline "moo") '("foo" (br) "bar"))
  (check-equal? (convert-linebreaks '("foo" "\n\n" "bar")) '("foo" "\n\n" "bar")))



;; recursive whitespace test
(define/contract (whitespace? x)
  (any/c . -> . boolean?)
  (cond
    [(or (string? x) (symbol? x)) (->boolean (regexp-match #px"^\\s+$" (->string x)))]
    [(equal? "" x) #t] ; empty string is deemed whitespace
    [(or (list? x) (vector? x)) (andmap whitespace? (->list x))]
    [else #f]))


;; is x a paragraph break?
(define/contract (paragraph-break? x #:pattern [paragraph-pattern #px"^\n\n+$"])
  ((any/c) (#:pattern pregexp?) . ->* . boolean?)
  (and (string? x) (->boolean (regexp-match paragraph-pattern x))))

(module+ test
  (check-false (paragraph-break? "foo"))
  (check-false (paragraph-break? "\n"))
  (check-false (paragraph-break? "\n \n"))
  (check-true (paragraph-break? "\n \n" #:pattern #px"^\n \n$"))
  (check-true (paragraph-break? "\n\n"))
  (check-true (paragraph-break? "\n\n\n")))



;; Find adjacent newline characters in a list and merge them into one item
;; Scribble, by default, makes each newline a separate list item
;; In practice, this is worthless.
(define/contract (merge-newlines x)
  (list? . -> . list?)
  (define (newline? x)
    (and (string? x) (equal? "\n" x)))
  (define (not-newline? x)
    (not (newline? x)))
  
  (define (really-merge-newlines xs [acc '()])
    (if (empty? xs)
        acc
        ;; Try to peel the newlines off the front.
        (let-values ([(leading-newlines remainder) (splitf-at xs newline?)])
          (if (not (empty? leading-newlines)) ; if you got newlines ...
              ;; combine them into a string and append them to the accumulator, 
              ;; and recurse on the rest
              (really-merge-newlines remainder (append acc (list (string-join leading-newlines ""))))
              ;; otherwise peel off elements up to the next newline, append them to accumulator,
              ;; and recurse on the rest
              (really-merge-newlines (dropf remainder not-newline?) 
                                     (append acc (takef remainder not-newline?)))))))
  
  (cond
    [(list? x) (really-merge-newlines (map merge-newlines x))]
    [else x]))

(module+ test
  (check-equal? (merge-newlines '(p "\n" "foo" "\n" "\n" "bar" (em "\n" "\n" "\n"))) 
                '(p "\n" "foo" "\n\n" "bar" (em "\n\n\n"))))



;; todo: add native support for list-xexpr
;; decode triple newlines to list items


;; prepare elements for paragraph testing
(define/contract (prep-paragraph-flow xc)
  (txexpr-elements? . -> . txexpr-elements?)
  (convert-linebreaks (merge-newlines (trim xc whitespace?))))

(module+ test
  (check-equal? (prep-paragraph-flow '("\n" "foo" "\n" "\n" "bar" "\n" "ino" "\n"))
                '("foo" "\n\n" "bar" (br) "ino")))

;; apply paragraph tag
(define/contract (wrap-paragraph xc #:tag [tag 'p]) 
  ((txexpr-elements?) (#:tag symbol?) . ->* . block-xexpr?)
  (match xc
    [(list (? block-xexpr? bx)) bx] ; leave a single block xexpr alone
    [else (make-txexpr tag empty xc)])) ; otherwise wrap in p tag

(module+ test
  (check-equal? (wrap-paragraph '("foo" "bar")) '(p "foo" "bar"))
  (check-equal? (begin (append-block-tag 'para) (wrap-paragraph #:tag 'para '("foo" "bar"))) 
                '(para "foo" "bar"))
  (check-equal? (wrap-paragraph '((p "bar" "foo"))) '(p "bar" "foo"))
  (check-equal? (wrap-paragraph '((div "bar" "foo") "Hi" )) '(p (div "bar" "foo") "Hi")))


;; detect paragraphs
;; todo: unit tests
(define/contract (detect-paragraphs elements)
  (txexpr-elements? . -> . txexpr-elements?)
  (let ([elements (prep-paragraph-flow elements)]) 
    (if (ormap paragraph-break? elements) ; need this condition to prevent infinite recursion
        (map wrap-paragraph (splitf-at* elements paragraph-break?)) ; split into ¶¶
        elements)))