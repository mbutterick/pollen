#lang racket/base
(require racket/contract racket/list racket/string racket/match)
(require "../readability.rkt" "../predicates.rkt" "../tools.rkt")

(module+ test (require rackunit))

(provide (all-defined-out))

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
  ((tagged-xexpr?) (#:nbsp string? #:minimum-word-length integer?) . ->* . tagged-xexpr?)
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
      [(tagged-xexpr? x) 
       (let-values([(tag attr elements) (break-tagged-xexpr x)])
         (if (> (length elements) 0) ; elements is list of xexprs
             (let-values ([(all-but-last last) (split-at elements (sub1 (length elements)))]) 
               (make-tagged-xexpr tag attr `(,@all-but-last ,(find-last-word-space (car last)))))
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
  ((tagged-xexpr?) (#:single-prepend list? #:double-prepend list?) . ->* . tagged-xexpr?)
  (define two-or-more-char-string? (λ(i) (and (string? i) (>= (len i) 2))))
  (define-values (tag attr elements) (break-tagged-xexpr nx))
  (make-tagged-xexpr tag attr
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
                                                    [(? tagged-xexpr? nx) (wrap-hanging-quotes nx)]
                                                    [else (car elements)])])
                           (cons new-car-elements (cdr elements)))
                         elements)))



(module+ test
  (check-equal? (wrap-hanging-quotes '(p "\"Hi\" there")) '(p (dquo "“" "Hi\" there")))
  (check-equal? (wrap-hanging-quotes '(p "'Hi' there")) '(p (squo "‘" "Hi' there")))
  (check-equal? (wrap-hanging-quotes '(p "'Hi' there") #:single-prepend '(foo ((bar "ino")))) 
                '(p (foo ((bar "ino")) "‘" "Hi' there")))
  ;; make sure tagged-xexpr without elements passes through unscathed
  (check-equal? (wrap-hanging-quotes '(div ((style "height:2em")))) '(div ((style "height:2em")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lines, blocks, paragraphs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; turn the right items into <br> tags
(define/contract (convert-linebreaks xc #:newline [newline "\n"])
  ((xexpr-elements?) (#:newline string?) . ->* . xexpr-elements?)
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
  (xexpr-elements? . -> . xexpr-elements?)
  (convert-linebreaks (merge-newlines (trim xc whitespace?))))

(module+ test
  (check-equal? (prep-paragraph-flow '("\n" "foo" "\n" "\n" "bar" "\n" "ino" "\n"))
                '("foo" "\n\n" "bar" (br) "ino")))

;; apply paragraph tag
(define/contract (wrap-paragraph xc #:tag [tag 'p]) 
  ((xexpr-elements?) (#:tag symbol?) . ->* . block-xexpr?)
  (match xc
    [(list (? block-xexpr? bx)) bx] ; leave a single block xexpr alone
    [else (make-tagged-xexpr tag empty xc)])) ; otherwise wrap in p tag

(module+ test
  (check-equal? (wrap-paragraph '("foo" "bar")) '(p "foo" "bar"))
  (check-equal? (begin (append-block-tag 'para) (wrap-paragraph #:tag 'para '("foo" "bar"))) 
                '(para "foo" "bar"))
  (check-equal? (wrap-paragraph '((p "bar" "foo"))) '(p "bar" "foo"))
  (check-equal? (wrap-paragraph '((div "bar" "foo") "Hi" )) '(p (div "bar" "foo") "Hi")))


;; detect paragraphs
;; todo: unit tests
(define/contract (detect-paragraphs elements)
  (xexpr-elements? . -> . xexpr-elements?)
  (let ([elements (prep-paragraph-flow elements)]) 
    (if (ormap paragraph-break? elements) ; need this condition to prevent infinite recursion
        (map wrap-paragraph (splitf-at* elements paragraph-break?)) ; split into ¶¶
        elements)))