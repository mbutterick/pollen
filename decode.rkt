#lang racket/base
(require xml txexpr racket/string racket/match racket/list (prefix-in html: pollen/html) sugar/list sugar/container sugar/len sugar/define sugar/coerce sugar/test)
(require "debug.rkt" "world.rkt")


(define (symbols? x) (and (list? x) (andmap symbol? x)))

(define+provide (to-string x)
  (if (string? x)
      x ; fast exit for strings
      (with-handlers  ([exn:fail? (λ(exn) (error (format "Pollen decoder: can't convert ~v to ~a" x 'string)))])
        (cond
          [(equal? '() x) ""]
          [(symbol? x) (symbol->string x)]
          [(number? x) (number->string x)]
          [(path? x) (path->string x)]
          [(char? x) (format "~a" x)]
          [(void? x) ""]
          ;; todo: guard against weird shit like lists of procedures
          [(or (list? x) (hash? x) (vector? x)) (format "~v" x)] ; ok to convert datatypes
          [else (error)])))) ; but things like procedures should throw an error

(define decode-proc-output-contract (or/c xexpr? (non-empty-listof xexpr?)))

(define multiple-entity-signal (gensym "multiple-entity"))

(define (->list/tx x)
  ;; same as ->list but catches special case of single txexpr,
  ;; which is itself a list, but in this case should be wrapped into a list,
  ;; for use with append-map.
  (cond
    ;; use multiple-entity-signal to distinguish list of entities from txexprs
    ;; ambiguous example '(copy copy)
    ;; but with signal, it's '(multiple-entity-signal copy copy)
    [(and (pair? x) (eq? (car x) multiple-entity-signal)) (cdr x)]
    [(txexpr? x) (list x)]
    [else (->list x)]))


;; decoder wireframe
(define+provide/contract (decode tx-in
                                 #:txexpr-tag-proc [txexpr-tag-proc (λ(x)x)]
                                 #:txexpr-attrs-proc [txexpr-attrs-proc (λ(x)x)]
                                 #:txexpr-elements-proc [txexpr-elements-proc (λ(x)x)]
                                 #:block-txexpr-proc [block-txexpr-proc (λ(x)x)]
                                 #:inline-txexpr-proc [inline-txexpr-proc (λ(x)x)]
                                 #:string-proc [string-proc (λ(x)x)]
                                 #:entity-proc [entity-proc (λ(x)x)]
                                 #:cdata-proc [cdata-proc (λ(x)x)]
                                 #:exclude-tags [excluded-tags '()]
                                 #:exclude-attrs [excluded-attrs '()])
  ((xexpr/c)  
   (#:txexpr-tag-proc (txexpr-tag? . -> . txexpr-tag?)
                      #:txexpr-attrs-proc (txexpr-attrs? . -> . txexpr-attrs?)
                      #:txexpr-elements-proc (txexpr-elements? . -> . txexpr-elements?)
                      #:block-txexpr-proc (block-txexpr? . -> . decode-proc-output-contract)
                      #:inline-txexpr-proc (txexpr? . -> . decode-proc-output-contract)
                      #:string-proc (string? . -> . decode-proc-output-contract)
                      #:entity-proc ((or/c symbol? valid-char?) . -> . decode-proc-output-contract)
                      #:cdata-proc (cdata? . -> . decode-proc-output-contract)
                      #:exclude-tags (listof txexpr-tag?)
                      #:exclude-attrs txexpr-attrs?) . ->* . txexpr?)
  (let loop ([x tx-in])
    (cond
      [(txexpr? x) (let-values([(tag attrs elements) (txexpr->values x)]) 
                     (if (or (member tag excluded-tags) (ormap (λ(attr) (member attr excluded-attrs)) attrs)) 
                         x ; because it's excluded
                         ;; we apply processing here rather than do recursive descent on the pieces
                         ;; because if we send them back through loop, certain element types are ambiguous
                         ;; e.g., ((p "foo")) tests out as both txexpr-attrs and txexpr-elements
                         (let ([decoded-txexpr 
                                (apply make-txexpr (list (txexpr-tag-proc tag) 
                                                         (txexpr-attrs-proc attrs) 
                                                         (txexpr-elements-proc (append-map (compose1 ->list/tx loop) elements))))])
                           ((if (block-txexpr? decoded-txexpr)
                                block-txexpr-proc
                                inline-txexpr-proc) decoded-txexpr))))]
      [(string? x) (string-proc x)]
      [(or (symbol? x) (valid-char? x))
       (define result (entity-proc x))
       (if (list? result)
           ;; add a signal to list of multiple entities to avoid downstream ambiguity with txexpr
           ;; for instance '(copy copy) is a list of entities, but also a txexpr
           ;; stick a signal on the front, which will be picked up later
           (cons multiple-entity-signal result)
           result)]
      [(cdata? x) (cdata-proc x)]
      [else (error "decode: can't decode" x)])))

(module-test-external
 (require racket/list txexpr racket/function)
 (define (doubler x) (list x x)) 
 (check-equal? (decode #:txexpr-elements-proc identity '(p "foo")) '(p "foo"))
 ;; can't use doubler on txexpr-elements because it needs a list, not list of lists
 (check-equal? (decode #:txexpr-elements-proc (λ(elems) (append elems elems)) '(p "foo")) '(p "foo" "foo"))
 (check-equal? (decode #:block-txexpr-proc identity '(p "foo")) '(p "foo"))
 (check-equal? (decode #:block-txexpr-proc doubler '(p "foo")) (list '(p "foo") '(p "foo")))
 (check-equal? (decode #:inline-txexpr-proc identity '(p (span "foo"))) '(p (span "foo")))
 (check-equal? (decode #:inline-txexpr-proc doubler '(p (span "foo"))) '(p (span "foo") (span "foo")))
 (check-equal? (decode #:string-proc identity '(p (span "foo"))) '(p (span "foo")))
 (check-equal? (decode #:string-proc doubler '(p (span "foo"))) '(p (span "foo" "foo")))
 (check-equal? (decode #:entity-proc identity '(p (span "foo" 'amp))) '(p (span "foo" 'amp)))
 (check-equal? (decode #:entity-proc identity '(p 42)) '(p 42))
 (check-equal? (decode #:entity-proc doubler '(p 42)) '(p 42 42))
 (check-equal? (decode #:entity-proc identity '(p amp)) '(p amp))
 (check-equal? (decode #:entity-proc doubler '(p amp)) '(p amp amp))
 (check-equal? (decode-elements #:string-proc identity '("foo")) '("foo"))
 (check-equal? (decode-elements #:string-proc doubler '("foo")) '("foo" "foo")))

;; it would be nice to not repeat this, but with all the keywords, it's simpler to repeat than do a macro
(define+provide/contract (decode-elements elements
                                          #:txexpr-tag-proc [txexpr-tag-proc (λ(x)x)]
                                          #:txexpr-attrs-proc [txexpr-attrs-proc (λ(x)x)]
                                          #:txexpr-elements-proc [txexpr-elements-proc (λ(x)x)]
                                          #:block-txexpr-proc [block-txexpr-proc (λ(x)x)]
                                          #:inline-txexpr-proc [inline-txexpr-proc (λ(x)x)]
                                          #:string-proc [string-proc (λ(x)x)]
                                          #:entity-proc [entity-proc (λ(x)x)]
                                          #:cdata-proc [cdata-proc (λ(x)x)]
                                          #:exclude-tags [excluded-tags '()]
                                          #:exclude-attrs [excluded-attrs '()])
  ((txexpr-elements?)  
   (#:txexpr-tag-proc (txexpr-tag? . -> . txexpr-tag?)
                      #:txexpr-attrs-proc (txexpr-attrs? . -> . txexpr-attrs?)
                      #:txexpr-elements-proc (txexpr-elements? . -> . txexpr-elements?)
                      #:block-txexpr-proc (block-txexpr? . -> . decode-proc-output-contract)
                      #:inline-txexpr-proc (txexpr? . -> . decode-proc-output-contract)
                      #:string-proc (string? . -> . decode-proc-output-contract)
                      #:entity-proc ((or/c symbol? valid-char?) . -> . decode-proc-output-contract)
                      #:cdata-proc (cdata? . -> . decode-proc-output-contract)
                      #:exclude-tags (listof txexpr-tag?)
                      #:exclude-attrs txexpr-attrs?) . ->* . txexpr?)
  
  (define temp-tag (gensym "temp-tag"))
  (define decode-result (decode `(temp-tag ,@elements)
                                #:txexpr-tag-proc txexpr-tag-proc
                                #:txexpr-attrs-proc txexpr-attrs-proc
                                #:txexpr-elements-proc txexpr-elements-proc
                                #:block-txexpr-proc block-txexpr-proc
                                #:inline-txexpr-proc inline-txexpr-proc
                                #:string-proc string-proc
                                #:entity-proc entity-proc
                                #:cdata-proc cdata-proc
                                #:exclude-tags excluded-tags
                                #:exclude-attrs excluded-attrs))
  (get-elements decode-result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Blocks

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial set of block tags: from html
(define+provide project-block-tags 
  (make-parameter html:block-tags))


;; tags are inline unless they're registered as block tags.
(define+provide/contract (block-txexpr? x)
  (any/c . -> . boolean?)
  (and (txexpr? x) (member (get-tag x) (project-block-tags)) #t))


(define+provide/contract (register-block-tag tag)
  (txexpr-tag? . -> . void?)
  (project-block-tags (cons tag (project-block-tags))))

(module-test-external
 (check-true (begin (register-block-tag 'barfoo) (block-txexpr? '(barfoo "foo")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Typography

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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

(module-test-external
 (check-equal? (smart-dashes "I had --- maybe 13 -- 20 --- hob-nobs.") "I had—maybe 13–20—hob-nobs.")
 (check-equal? (smart-quotes "\"Why,\" she could've asked, \"are we in O‘ahu watching 'Mame'?\"") 
               "“Why,” she could’ve asked, “are we in O‘ahu watching ‘Mame’?”")
 (check-equal? (smart-quotes "\"\'Impossible.\' Yes.\"") "“‘Impossible.’ Yes.”"))


(define+provide/contract (smart-quotes str)
  (string? . -> . string?)
  
  (define quotes
    '((#px"(?<=\\w)'(?=\\w)" "’") ; apostrophe
      (#px"(?<!\\w)'(?=\\S)" "‘") ; single_at_beginning
      (#px"(?<=\\S)'(?!\\w)" "’") ; single_at_end
      (#px"(?<!\\w)\"(?=\\S)" "“") ; double_at_beginning
      (#px"(?<=\\S)\"(?!\\w)" "”"))) ; double_at_end
  
  ((make-replacer quotes) str))



;; insert nbsp between last two words
(define+provide/contract (nonbreaking-last-space x #:nbsp [nbsp (->string #\u00A0)] 
                                                 #:minimum-word-length [minimum-word-length 6]
                                                 #:last-word-proc [last-word-proc (λ(x) x)])
  ((txexpr?) (#:nbsp string? #:minimum-word-length integer? #:last-word-proc procedure?) . ->* . txexpr?)
  
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
          (define last-word (list->string (reverse last-word-chars)))
          `(,front-chars ,(last-word-proc last-word))) ; don't concatenate last word bc last-word-proc might be a txexpr wrapper
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

(module-test-external
 ;; todo: make some tougher tests, it gets flaky with edge cases
 (check-equal? (nonbreaking-last-space '(p "Hi there")) '(p "Hi " "there")) ; nbsp in between last two words
 (check-equal? (nonbreaking-last-space '(p "Hi there") #:nbsp "Ø") '(p "HiØ" "there")) ; but let's make it visible
 (check-equal? (nonbreaking-last-space '(p "Hi there") #:nbsp "_up_") '(p "Hi_up_" "there")) 
 (check-equal? (nonbreaking-last-space '(p "Hi there") #:nbsp "_up_" #:minimum-word-length 3) 
               '(p "Hi " "there")) 
 (check-equal? (nonbreaking-last-space '(p "Hi here" (em "ho there")) #:nbsp "Ø") '(p "Hi here" (em "hoØ" "there"))))

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
                                               (define str-rest (get tcs 1 (len tcs)))
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

(module-test-external
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
(define+provide/contract (detect-linebreaks xc 
                                            #:separator [newline (world:current-linebreak-separator)]
                                            #:insert [linebreak '(br)])
  ((txexpr-elements?) (#:separator string? #:insert xexpr?) . ->* . txexpr-elements?)
  ;; todo: should this test be not block + not whitespace?
  (define not-block? (λ(i) (not (block-txexpr? i))))
  (filter-not empty?
              (for/list ([i (in-range (len xc))])
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

(module-test-external
 (check-equal? (detect-linebreaks '("foo" "\n" "bar")) '("foo" (br) "bar"))
 (check-equal? (detect-linebreaks '("\n" "foo" "\n" "bar" "\n")) '("\n" "foo" (br) "bar" "\n"))
 (check-equal? (detect-linebreaks '((p "foo") "\n" (p "bar"))) '((p "foo") (p "bar")))
 (check-equal? (detect-linebreaks '("foo" "\n" (p "bar"))) '("foo" (p "bar")))
 (check-equal? (detect-linebreaks '("foo" "moo" "bar")) '("foo" "moo" "bar"))
 (check-equal? (detect-linebreaks '("foo" "moo" "bar") #:insert "moo") '("foo" "moo" "bar"))
 (check-equal? (detect-linebreaks '("foo" "\n\n" "bar")) '("foo" "\n\n" "bar")))


(define+provide/contract (whitespace? x [nbsp? #f])
  ((any/c)(boolean?) . ->* . coerce/boolean?)
  (define pat (pregexp (format "^[\\s~a]+$" (if nbsp? #\u00A0 ""))))
  (cond
    [(equal? "" x) #t] ; empty string is deemed whitespace
    [(or (string? x) (symbol? x)) (regexp-match pat (->string x))]
    [(or (list? x) (vector? x)) (and (not (empty? x)) (andmap (λ(i) (whitespace? i nbsp?)) (->list x)))] ; andmap returns #t for empty lists
    [else #f]))

(module-test-external
 (require racket/format)
 (check-true (whitespace? " "))
 (check-false (whitespace? (~a #\u00A0)))
 (check-true (whitespace/nbsp? (~a #\u00A0)))
 (check-true (whitespace/nbsp? (vector (~a #\u00A0))))
 (check-false (whitespace? (format " ~a " #\u00A0)))
 (check-true (whitespace/nbsp? (format " ~a " #\u00A0))))


(define+provide/contract (whitespace/nbsp? x)
  (any/c . -> . coerce/boolean?)
  (whitespace? x #t))


;; is x a paragraph break?
(define+provide/contract (paragraph-break? x #:separator [sep (world:current-paragraph-separator)])
  ((any/c) (#:separator pregexp?) . ->* . coerce/boolean?)
  (define paragraph-pattern (pregexp (format "^~a+$" sep)))
  (and (string? x) (regexp-match paragraph-pattern x)))



(define (newline? x)
  (and (string? x) (equal? (world:current-newline) x)))
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

(module-test-external
 (check-equal? (merge-newlines '(p "\n" "foo" "\n" "\n" "bar" (em "\n" "\n" "\n"))) 
               '(p "\n" "foo" "\n\n" "bar" (em "\n\n\n"))))



;; detect paragraphs
;; todo: unit tests
(define+provide/contract (detect-paragraphs elements #:tag [tag 'p]
                                            #:separator [sep (world:current-paragraph-separator)]
                                            #:linebreak-proc [linebreak-proc detect-linebreaks]
                                            #:force? [force-paragraph #f])
  ((txexpr-elements?) (#:tag symbol? #:separator string? #:linebreak-proc (txexpr-elements? . -> . txexpr-elements?) #:force? boolean?) 
                      . ->* . txexpr-elements?)
  
  ;; prepare elements for paragraph testing
  (define (prep-paragraph-flow elems)
    (linebreak-proc (merge-newlines (trimf elems whitespace?))))
  
  (define my-paragraph-break? (λ(x) (and (paragraph-break? x #:separator sep) #t)))
  
  (define (wrap-paragraph elems) 
    (match elems
      [(list (? block-txexpr? bxs) ...) bxs] ; leave a series of block xexprs alone
      [else (list (make-txexpr tag empty elems))])) ; otherwise wrap in p tag
  
  (let ([elements (prep-paragraph-flow elements)])
    (define explicit-or-implicit-paragraph-break? (λ(x) (or (my-paragraph-break? x) (block-txexpr? x))))
    (if (ormap explicit-or-implicit-paragraph-break? elements) ; need this condition to prevent infinite recursion
        ;; use append-map on wrap-paragraph rather than map to permit return of multiple elements
        (append-map wrap-paragraph (append-map (λ(es) (filter-split es my-paragraph-break?)) (slicef elements block-txexpr?))) ; split into ¶¶, using both implied and explicit paragraph breaks
        (if force-paragraph
            (append-map wrap-paragraph (slicef elements block-txexpr?)) ; upconverts non-block elements to paragraphs
            elements))))                

(module-test-external
 (check-equal? (detect-paragraphs '("First para" "\n\n" "Second para"))
               '((p "First para") (p "Second para")))
 (check-equal? (detect-paragraphs '("First para" "\n\n" "Second para" "\n" "Second line"))
               '((p "First para") (p "Second para" (br) "Second line")))
 (check-equal? (detect-paragraphs '("First para" "\n\n" (div "Second block")))
               '((p "First para") (div "Second block")))
 (check-equal? (detect-paragraphs '((div "First block") "\n\n" (div "Second block")))
               '((div "First block") (div "Second block")))
 (check-equal? (detect-paragraphs '("First para" "\n\n" "Second para") #:tag 'ns:p)
               '((ns:p "First para") (ns:p "Second para")))
 (check-equal? (detect-paragraphs '("First para" "\n\n" "Second para" "\n" "Second line")
                                  #:linebreak-proc (λ(x) (detect-linebreaks x #:insert '(newline))))
               '((p "First para") (p "Second para" (newline) "Second line")))
 (check-equal? (detect-paragraphs '("foo" "\n\n" (div "bar") (div "zam")))
               '((p "foo") (div "bar") (div "zam")))
 (check-equal? (detect-paragraphs '("foo" "\n\n" (div "bar") "\n\n" (div "zam")))
               '((p "foo") (div "bar") (div "zam")))
 
 (check-equal? (detect-paragraphs '("foo")) '("foo"))
 (check-equal? (detect-paragraphs '("foo") #:force? #t) '((p "foo")))
 (check-equal? (detect-paragraphs '((div "foo"))) '((div "foo")))
 (check-equal? (detect-paragraphs '((div "foo")) #:force? #t) '((div "foo")))
 (check-equal? (detect-paragraphs '("foo" "\n\n" (div "bar"))) '((p "foo") (div "bar")))
 (check-equal? (detect-paragraphs '("foo" (div "bar"))) '((p "foo") (div "bar")))
 (check-equal? (detect-paragraphs '("foo" (div "bar")) #:force? #t) '((p "foo") (div "bar")))
 (check-equal? (detect-paragraphs '("foo" (div "bar") "zam")) '((p "foo") (div "bar") (p "zam")))
 (check-equal? (detect-paragraphs '("foo" (span "zing") (div "bar") "zam")) '((p "foo" (span "zing")) (div "bar") (p "zam")))
 (check-equal? (detect-paragraphs '("foo" (span "zing") (div "bar") "zam") #:force? #t) '((p "foo" (span "zing")) (div "bar") (p "zam"))))