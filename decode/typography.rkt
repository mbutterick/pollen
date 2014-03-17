#lang racket/base
(require racket/list racket/match xml)
(require "block.rkt" "../world.rkt" sugar txexpr)


(define (make-replacer query+replacement)
  (let ([queries (map car query+replacement)]
        [replacements (map second query+replacement)])
    ;; reverse because first in list should be first applied to str (and compose1 works right-to-left)
    (apply compose1 (reverse (map (λ(query replacement) (λ(str) (regexp-replace* query str replacement))) queries replacements)))))

(define+provide/contract (smart-dashes str)
  (string? . -> . string?)
  
  (define dashes 
    ;; fix em dashes first, else they'll be mistaken for en dashes
    ;; \\s is whitespace + #\u00A0 is nonbreaking space
    '((#px"[\\s#\u00A0]*(---|—)[\\s#\u00A0]*" "—") ; em dash
      (#px"[\\s#\u00A0]*(--|–)[\\s#\u00A0]*" "–"))) ; en dash
  
  
  ((make-replacer dashes) str))


(define+provide/contract (smart-quotes str)
  (string? . -> . string?)
  
  (define quotes
    '((#px"(?<=\\w)'(?=\\w)" "’") ; apostrophe
      (#px"(?<!\\w)'(?=\\w)" "‘") ; single_at_beginning
      (#px"(?<=\\S)'(?!\\w)" "’") ; single_at_end
      (#px"(?<!\\w)\"(?=\\w)" "“") ; double_at_beginning
      (#px"(?<=\\S)\"(?!\\w)" "”"))) ; double_at_end
  
  ((make-replacer quotes) str))



;; insert nbsp between last two words
(define+provide/contract (nonbreaking-last-space x #:nbsp [nbsp (->string #\u00A0)] 
                                                 #:minimum-word-length [minimum-word-length 6])
  ((txexpr?) (#:nbsp string? #:minimum-word-length integer?) . ->* . txexpr?)
  
  ;; todo: parameterize this, as it will be different for each project
  (define tags-to-pay-attention-to '(p aside)) ; only apply to paragraphs
  
  (define (replace-last-space str)
    (if (#\space . in? . str)
        (let ([reversed-str-list (reverse (string->list str))]
              [reversed-nbsp (reverse (string->list (->string nbsp)))])
          (define-values (last-word-chars other-chars) 
            (splitf-at reversed-str-list (λ(i) (not (eq? i #\space)))))
          
          (define front-chars (if (< (len last-word-chars) minimum-word-length) ; OK for long words to be on their own line
                                  ; first char of other-chars will be the space, so use cdr
                                  (string-append (list->string (reverse (cdr other-chars))) (->string nbsp))
                                  (list->string (reverse other-chars))))
          `(,front-chars (span [[pollen "no-hyphens"]] ,(list->string (reverse last-word-chars)))))
        (list str)))
  
  (define (find-last-word-space x) ; recursively traverse xexpr
    (cond
      [(string? x) (replace-last-space x)] ; todo: this assumes a paragraph only has one string in it.
      [(txexpr? x)
       (let-values([(tag attr elements) (txexpr->values x)])
         (if (> (length elements) 0) ; elements is list of xexprs
             (let-values ([(all-but-last last) (split-at elements (sub1 (length elements)))]) 
               (define result (find-last-word-space (car last)))
               (define result-items (if (txexpr? result) (list result) result)) ; might be txexpr, or list of new elements
               (make-txexpr tag attr `(,@all-but-last ,@result-items)))
             x))]
      [else x]))
  
  (if ((car x) . in? . tags-to-pay-attention-to)
      (find-last-word-space x)
      x))


; wrap initial quotes for hanging punctuation
; todo: improve this
; does not handle <p>“<em>thing</em> properly
(define+provide/contract (wrap-hanging-quotes nx 
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






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lines, blocks, paragraphs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; turn the right items into <br> tags
(define+provide/contract (detect-linebreaks xc 
                                             #:separator [newline world:linebreak-separator]
                                             #:insert [linebreak '(br)])
  ((txexpr-elements?) (#:separator string? #:insert xexpr?) . ->* . txexpr-elements?)
  ;; todo: should this test be not block + not whitespace?
  (define not-block? (λ(i) (not (block-txexpr? i))))
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
                       [(list (? not-block?) newline (? not-block?)) linebreak]
                       [else empty])] ; otherwise delete
                    [else item])))))



;; recursive whitespace test
(define+provide/contract (whitespace? x)  
  (any/c . -> . coerce/boolean?)
  (cond
    [(equal? "" x) #t] ; empty string is deemed whitespace
    [(or (string? x) (symbol? x)) (regexp-match #px"^\\s+$" (->string x))]
    [(or (list? x) (vector? x)) (andmap whitespace? (->list x))]
    [else #f]))


(define+provide/contract (whitespace/nbsp? x)  
  (any/c . -> . coerce/boolean?)
  (or (whitespace? x) (equal? (->string x) (->string #\u00A0))))

;; is x a paragraph break?
(define+provide/contract (paragraph-break? x #:separator [sep world:paragraph-separator])
  ((any/c) (#:separator pregexp?) . ->* . coerce/boolean?)
  (define paragraph-pattern (pregexp (format "^~a+$" sep)))
  (and (string? x) (regexp-match paragraph-pattern x)))



(define (newline? x)
  (and (string? x) (equal? world:newline x)))
(define (not-newline? x)
  (not (newline? x)))

(define (do-merge xs [acc '()])
  (if (empty? xs)
      acc
      ;; Try to peel the newlines off the front.
      (let-values ([(leading-newlines remainder) (splitf-at xs newline?)])
        (if (not (empty? leading-newlines)) ; if you got newlines ...
            ;; combine them into a string and append them to the accumulator, 
            ;; and recurse on the rest
            (do-merge remainder (append acc (list (apply string-append leading-newlines))))
            ;; otherwise peel off elements up to the next newline, append them to accumulator,
            ;; and recurse on the rest
            (do-merge (dropf remainder not-newline?) 
                      (append acc (takef remainder not-newline?)))))))


;; Find adjacent newline characters in a list and merge them into one item
;; Scribble, by default, makes each newline a separate list item
;; In practice, this is worthless.
(define+provide/contract (merge-newlines x)
  (txexpr-elements? . -> . txexpr-elements?)  
  (cond
    [(list? x) (do-merge (map merge-newlines x))]
    [else x]))





;; detect paragraphs
;; todo: unit tests
(define+provide/contract (detect-paragraphs elements #:tag [tag 'p]
                                            #:separator [sep world:paragraph-separator]
                                            #:linebreak-proc [linebreak-proc detect-linebreaks])
  ((txexpr-elements?) (#:tag symbol? #:separator string? #:linebreak-proc (txexpr-elements? . -> . txexpr-elements?)) 
   . ->* . txexpr-elements?)
  
  ;; prepare elements for paragraph testing
  (define (prep-paragraph-flow xc)
    (linebreak-proc (merge-newlines (trim xc whitespace?))))
  
  
  (define my-paragraph-break? (λ(x) (and (paragraph-break? x #:separator sep) #t)))
  
  (define (wrap-paragraph xc) 
    (match xc
      [(list (? block-txexpr? bx)) bx] ; leave a single block xexpr alone
      [else (make-txexpr tag empty xc)])) ; otherwise wrap in p tag
  
  
  (let ([elements (prep-paragraph-flow elements)]) 
    (if (ormap my-paragraph-break? elements) ; need this condition to prevent infinite recursion
        (map wrap-paragraph (splitf-at* elements my-paragraph-break?)) ; split into ¶¶
        elements)))