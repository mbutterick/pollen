#lang racket/base
(require racket/contract racket/list racket/match)
(require (planet mb/pollen/tools) (planet mb/pollen/decode))

(provide (all-defined-out))

(module+ test (require rackunit))

(register-block-name 'boqi)

;; todo: contracts & unit tests
(define/contract (meta-proc meta)
  (meta-xexpr? . -> . named-xexpr?)
  `(meta ((name ,(second meta))(content ,(third meta)))))

(module+ test
  (check-equal? (meta-proc '(meta "key" "value")) '(meta ((name "key")(content "value")))))

;; is x a paragraph break?
(define/contract (paragraph-break? x)
  (any/c . -> . boolean?)
  ;; paragraph break = a string of two or more newlines
  (and (string? x) (>= (len x) 2) (equal? x (make-string (len x) #\newline))))

(module+ test
  (check-false (paragraph-break? "foo"))
  (check-false (paragraph-break? "\n"))
  (check-false (paragraph-break? "\n \n"))
  (check-true (paragraph-break? "\n\n"))
  (check-true (paragraph-break? "\n\n\n")))


;; convert single newline to br tag
;; only if neither adjacent tag is a block
;; otherwise delete
;; todo: contracts & unit tests
(define (convert-linebreaks x)  ; x is list
  (filter-not empty?
              (for/list ([i (len x)])
                (cond
                  [(equal? (get x i) "\n") ; todo: don't hardcode this
                   (if (andmap (λ(i) (not (block-xexpr? i))) (list (get x (sub1 i)) (get x (add1 i))))
                       '(br)
                       '())]
                  [else (get x i)]))))

;; todo: contracts & unit tests
(define (prep-paragraph-flow x)
  (convert-linebreaks (merge-newlines (trim x whitespace?))))

(module+ test
  (check-equal? (prep-paragraph-flow '("\n" "foo" "\n" "\n" "bar" "\n" "ino" "\n"))
                '("foo" "\n\n" "bar" (br) "ino")))

;; todo: contracts & unit tests
(define (wrap-paragraph x) ; x is a list containing paragraph pieces
  ; if paragraph is just one block-level xexpr
  (if (and (= (length x) 1) (block-xexpr? (car x))) 
      (car x) ; leave it
      `(p ,@x))) ; otherwise wrap in p tag

;; todo: contracts & unit tests
(define (xexpr-content-proc content)
  (let ([content (prep-paragraph-flow content)]) 
    (if (ormap paragraph-break? content) ; need this condition to prevent infinite recursion
        (map wrap-paragraph (splitf-at* content paragraph-break?)) ; split into ¶¶
        content)))


;; insert nbsp between last two words
(define/contract (nonbreaking-last-space x #:nbsp-char [nbsp #\ ])
  ((named-xexpr?) (#:nbsp-char char?) . ->* . named-xexpr?)
  (define minimum-word-length (add1 5)) ; add1 to account for final punctuation
  ; todo: parameterize this, as it will be different for each project
  (define tags-to-pay-attention-to '(p aside)) ; only apply to paragraphs
  
  (define (replace-last-space str)
    (if (#\space . in . str)
        (let ([reversed-str-list (reverse (string->list str))])
          (define-values (last-word-chars other-chars) 
            (splitf-at reversed-str-list (λ(i) (not (eq? i #\space)))))
          (list->string (reverse (append last-word-chars 
                                         ; OK for long words to be on their own line.
                                         (if (< (len last-word-chars) minimum-word-length)
                                             ; first char of other-chars will be the space, so use cdr
                                             (cons nbsp (cdr other-chars))
                                             other-chars)))))
        str))
  
  (define (find-last-word-space x) ; recursively traverse xexpr
    (cond
      [(string? x) (replace-last-space x)] 
      [(named-xexpr? x) 
       (let-values([(name attr content) (break-named-xexpr x)])
         (if (> (length content) 0) ; content is list of xexprs
             (let-values ([(all-but-last last) (split-at content (sub1 (length content)))]) 
               (make-named-xexpr name attr `(,@all-but-last ,(find-last-word-space (car last)))))
             x))]
      [else x]))
  
  (if ((car x) . in . tags-to-pay-attention-to)
      (find-last-word-space x)
      x))

;; todo: make some tougher tests, it gets flaky with edge cases
(module+ test
  (check-equal? (nonbreaking-last-space '(p "Hi there")) '(p "Hi there")) ; nbsp in between last two words
  (check-equal? (nonbreaking-last-space '(p "Hi there") #:nbsp-char #\Ø) '(p "HiØthere")) ; but let's make it visible
  (check-equal? (nonbreaking-last-space '(p "Hi here" (em "ho there")) #:nbsp-char #\Ø) '(p "Hi here" (em "hoØthere")))) 


; wrap initial quotes for hanging punctuation
; todo: improve this
; does not handle <p>“<em>thing</em> properly
(define/contract (wrap-hanging-quotes nx)
  (named-xexpr? . -> . named-xexpr?)
  (define-values (name attr content) (break-named-xexpr nx))
  (cond 
    [(and (not (empty? content)) 
          (string? (car content)) 
          (> (string-length (car content)) 1))
     (let ([new-car 
            (letrec ([x (car content)] 
                     [first (get x 0)] 
                     [rest (get x 1 'end)])
              (cond
                [(first . in . '("\"" "“"))
                 ; this has to be span so that it's explicitly 
                 ; an inline element. If not,
                 ; things like linebreak detection won't work.
                 `(span ((class "dquo")) ,(->string #\“) ,rest)]
                [(first . in . '("\'" "‘")) 
                 `(span ((class "squo")) ,(->string #\‘) ,rest)]
                [else x]))])
       (make-named-xexpr name attr (cons new-car (cdr content))))]
    [(and content (not (empty? content)) (named-xexpr? (car content)))
     (make-named-xexpr name attr (cons (wrap-hanging-quotes (car content)) (cdr content)))]
    [else nx]))


(module+ test
  (check-equal? (wrap-hanging-quotes '(p "\"Hi\" there")) '(p (span ((class "dquo")) "“" "Hi\" there")))
  (check-equal? (wrap-hanging-quotes '(p "'Hi' there")) '(p (span ((class "squo")) "‘" "Hi' there"))))


(define (block-xexpr-proc bx)
  (named-xexpr? . -> . named-xexpr?)
  (wrap-hanging-quotes (nonbreaking-last-space bx)))


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
    ;; [\\s ] is whitespace + nonbreaking space
    '((#px"[\\s ]*(---|—)[\\s ]*" "—") ; em dash
      (#px"[\\s ]*(--|–)[\\s ]*" "–"))) ; en dash
  
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


(define (string-proc str)
  (string? . -> . string?)
  (typogrify str))

(define (root . items)
  (named-xexpr? . -> . named-xexpr?)
  (decode (cons 'root items)
          ;          #:exclude-xexpr-names 'em
          ;          #:xexpr-name-proc [xexpr-name-proc (λ(x)x)]
          ;          #:xexpr-attr-proc [xexpr-attr-proc (λ(x)x)]
          #:xexpr-content-proc xexpr-content-proc
          #:block-xexpr-proc block-xexpr-proc
          ;          #:inline-xexpr-proc [inline-xexpr-proc (λ(x)x)]
          #:string-proc string-proc
          #:meta-proc meta-proc
          ))


(define foo "bar")