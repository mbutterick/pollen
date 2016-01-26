#lang racket/base
(require xml txexpr racket/list sugar/list sugar/define sugar/test)
(require "world.rkt"
         "private/whitespace.rkt")


(define (->list/tx x)
  ;; same as ->list but catches special case of single txexpr,
  ;; which is itself a list, but in this case should be wrapped into a list,
  ;; for use with append-map.
  (cond
    [(txexpr? x) (list x)]
    [(list? x) x]
    [else (list x)]))

(define decode-proc-output-contract (or/c txexpr-element? txexpr-elements?))
(define identity (λ(x) x))

;; decoder wireframe
(define+provide/contract (decode tx-in
                                 #:txexpr-tag-proc [txexpr-tag-proc identity]
                                 #:txexpr-attrs-proc [txexpr-attrs-proc identity]
                                 #:txexpr-elements-proc [txexpr-elements-proc identity]
                                 #:txexpr-proc [txexpr-proc identity]
                                 #:block-txexpr-proc [block-txexpr-proc identity]
                                 #:inline-txexpr-proc [inline-txexpr-proc identity]
                                 #:string-proc [string-proc identity]
                                 #:entity-proc [entity-proc identity]
                                 #:cdata-proc [cdata-proc identity]
                                 #:exclude-tags [excluded-tags empty]
                                 #:exclude-attrs [excluded-attrs empty])
  ((xexpr/c)  
   (#:txexpr-tag-proc (txexpr-tag? . -> . txexpr-tag?)
                      #:txexpr-attrs-proc (txexpr-attrs? . -> . txexpr-attrs?)
                      #:txexpr-elements-proc (txexpr-elements? . -> . txexpr-elements?)
                      #:txexpr-proc (txexpr? . -> . decode-proc-output-contract)
                      #:block-txexpr-proc (block-txexpr? . -> . decode-proc-output-contract)
                      #:inline-txexpr-proc (txexpr? . -> . decode-proc-output-contract)
                      #:string-proc (string? . -> . decode-proc-output-contract)
                      #:entity-proc ((or/c symbol? valid-char?) . -> . decode-proc-output-contract)
                      #:cdata-proc (cdata? . -> . decode-proc-output-contract)
                      #:exclude-tags txexpr-tags?
                      #:exclude-attrs txexpr-attrs?) . ->* . decode-proc-output-contract)
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
                           ((compose1 txexpr-proc (if (block-txexpr? decoded-txexpr)
                                                      block-txexpr-proc
                                                      inline-txexpr-proc)) decoded-txexpr))))]
      [(string? x) (string-proc x)]
      [(or (symbol? x) (valid-char? x)) (entity-proc x)]
      [(cdata? x) (cdata-proc x)]
      [else (error "decode: can't decode" x)])))

(module-test-external
 (require racket/list txexpr racket/function)
 (define (doubler x) (list x x))
 (define (doubletag x) (txexpr (string->symbol (format "~a~a" (get-tag x) (get-tag x))) (get-attrs x) (get-elements x)))
 (check-equal? (decode #:txexpr-elements-proc identity '(p "foo")) '(p "foo"))
 ;; can't use doubler on txexpr-elements because it needs a list, not list of lists
 (check-equal? (decode #:txexpr-elements-proc (λ(elems) (append elems elems)) '(p "foo")) '(p "foo" "foo"))
 (check-equal? (decode #:block-txexpr-proc identity '(p "foo")) '(p "foo"))
 (check-equal? (decode #:block-txexpr-proc doubler '(p "foo")) (list '(p "foo") '(p "foo")))
 (check-equal? (decode #:block-txexpr-proc doubler '(p "foo")) (list '(p "foo") '(p "foo")))
 (check-equal? (decode #:txexpr-proc doubletag '(root (p "foo") (b "bar"))) '(rootroot (pp "foo") (bb "bar")))
 (check-equal? (decode #:block-txexpr-proc doubletag '(root (p "foo") (b "bar"))) '(rootroot (pp "foo") (b "bar")))
 (check-equal? (decode #:inline-txexpr-proc doubletag '(root (p "foo") (b "bar"))) '(root (p "foo") (bb "bar")))
 (check-equal? (decode #:inline-txexpr-proc identity '(p (span "foo"))) '(p (span "foo")))
 (check-equal? (decode #:inline-txexpr-proc doubler '(p (span "foo"))) '(p (span "foo") (span "foo")))
 (check-equal? (decode #:string-proc identity '(p (span "foo"))) '(p (span "foo")))
 (check-equal? (decode #:string-proc doubler '(p (span "foo"))) '(p (span "foo" "foo")))
 (check-equal? (decode #:entity-proc identity '(p (span "foo" 'amp))) '(p (span "foo" 'amp)))
 (check-equal? (decode #:entity-proc identity '(p 42)) '(p 42))
 (check-equal? (decode #:entity-proc doubler '(p 42)) '(p 42 42))
 (check-equal? (decode #:entity-proc identity '(p amp)) '(p amp))
 ;; next text doesn't work because list of symbol elements is ambiguous with tagged X-expression
 ;; is there a general patch for this? maybe, but for now it's better to not patch selectively
 ;; otherwise ambiguous expressions will have erratic misbehavior (instead of merely consistent misbehavior)
 ;;(check-equal? (decode #:entity-proc doubler '(p amp)) '(p amp amp))
 (check-equal? (decode-elements #:string-proc identity '("foo")) '("foo"))
 (check-equal? (decode-elements #:string-proc doubler '("foo")) '("foo" "foo")))

;; it would be nice to not repeat this, but with all the keywords, it's simpler to repeat than do a macro
(define+provide/contract decode-elements 
  ((txexpr-elements?)  
   (#:txexpr-tag-proc (txexpr-tag? . -> . txexpr-tag?)
                      #:txexpr-attrs-proc (txexpr-attrs? . -> . txexpr-attrs?)
                      #:txexpr-elements-proc (txexpr-elements? . -> . txexpr-elements?)
                      #:txexpr-proc (txexpr? . -> . decode-proc-output-contract)
                      #:block-txexpr-proc (block-txexpr? . -> . decode-proc-output-contract)
                      #:inline-txexpr-proc (txexpr? . -> . decode-proc-output-contract)
                      #:string-proc (string? . -> . decode-proc-output-contract)
                      #:entity-proc ((or/c symbol? valid-char?) . -> . decode-proc-output-contract)
                      #:cdata-proc (cdata? . -> . decode-proc-output-contract)
                      #:exclude-tags txexpr-tags?
                      #:exclude-attrs txexpr-attrs?) . ->* . decode-proc-output-contract)
  (make-keyword-procedure
   (λ (kws kwargs . args)
     (define temp-tag (gensym "temp-tag"))
     (define elements (car args))
     (define decode-result (keyword-apply decode kws kwargs (list (cons temp-tag elements))))
     (get-elements decode-result))))


(define+provide/contract (block-txexpr? x)
  (any/c . -> . boolean?)
  ;; Mostly this is used inside `decode`,
  ;; so rather than test for `txexpr?` at the beginning (which is potentially slow)
  ;; just look at the tag.
  (and (pair? x)
       (memq (get-tag x) (world:current-block-tags))
       #t))


(define+provide/contract (decode-linebreaks elems [maybe-linebreak-proc '(br)]
                                            #:separator [newline (world:current-linebreak-separator)])
  ((txexpr-elements?) ((or/c txexpr-element? (txexpr-element? txexpr-element? . -> . txexpr-element?)) #:separator string?) . ->* . txexpr-elements?)
  (define linebreak-proc (if (procedure? maybe-linebreak-proc)
                             maybe-linebreak-proc
                             (λ (e1 e2) maybe-linebreak-proc)))
  (define elems-vec (list->vector elems))
  (filter identity
          (for/list ([(item i) (in-indexed elems-vec)])
                    (cond
                      [(or (= i 0) (= i (sub1 (vector-length elems-vec)))) item] ; pass through first & last items
                      [(equal? item newline)
                       (let ([prev (vector-ref elems-vec (sub1 i))]
                             [next (vector-ref elems-vec (add1 i))])
                         ;; only convert if neither adjacent tag is a block
                         ;; (because blocks automatically force a newline before & after)
                         (if (or (block-txexpr? prev) (block-txexpr? next))
                             #f ; flag for filtering
                             (linebreak-proc prev next)))]
                      [else item]))))

(module-test-external
 (check-equal? (decode-linebreaks '("foo" "\n" "bar")) '("foo" (br) "bar"))
 (check-equal? (decode-linebreaks '("\n" "foo" "\n" "bar" "\n")) '("\n" "foo" (br) "bar" "\n"))
 (check-equal? (decode-linebreaks '((p "foo") "\n" (p "bar"))) '((p "foo") (p "bar")))
 (check-equal? (decode-linebreaks '("foo" "\n" (p "bar"))) '("foo" (p "bar")))
 (check-equal? (decode-linebreaks '("foo" "moo" "bar")) '("foo" "moo" "bar"))
 (check-equal? (decode-linebreaks '("foo" "moo" "bar") "moo") '("foo" "moo" "bar"))
 (check-equal? (decode-linebreaks '("foo" "\n\n" "bar")) '("foo" "\n\n" "bar")))


;; Find adjacent newline characters in a list and merge them into one item
;; Scribble, by default, makes each newline a separate list item.
(define+provide/contract (merge-newlines x)
  (txexpr-elements? . -> . txexpr-elements?)
  (define newline-pat (regexp (format "^~a+$" (world:current-newline))))
  (define (newlines? x) (and (string? x) (regexp-match newline-pat x)))  
  (define (merge-if-newlines xs)
    (if (newlines? (car xs))
        (list (apply string-append xs))
        xs))  
  (let loop ([x x])
    (if (pair? x)
        (let ([xs (map loop x)])
          (append-map merge-if-newlines (slicef xs newlines?)))
        x)))

(module-test-external
 (require racket/list)
 (check-equal? (merge-newlines empty) empty)
 (check-equal? (merge-newlines '(p "\n" "\n" "foo" "\n" "\n\n" "bar" (em "\n" "\n" "\n"))) 
               '(p "\n\n" "foo" "\n\n\n" "bar" (em "\n\n\n"))))



;; detect paragraphs
;; todo: unit tests
(define+provide/contract (decode-paragraphs elements [maybe-wrap-proc 'p]
                                            #:linebreak-proc [linebreak-proc decode-linebreaks]
                                            #:force? [force-paragraph #f])
  ((txexpr-elements?) ((or/c txexpr-tag? ((listof xexpr?) . -> . txexpr?))
                       #:linebreak-proc (txexpr-elements? . -> . txexpr-elements?)
                       #:force? boolean?) 
                      . ->* . txexpr-elements?)
  
  (define (prep-paragraph-flow elems)
    (linebreak-proc (merge-newlines (trimf elems whitespace?))))
  
  (define (paragraph-break? x)
    (define paragraph-separator (world:current-paragraph-separator))
    (define paragraph-pattern (pregexp (format "^~a+$" paragraph-separator)))
    (and (string? x) (regexp-match paragraph-pattern x)))
  
  (define (explicit-or-implicit-paragraph-break? x)
    (or (paragraph-break? x) (block-txexpr? x)))

  (define wrap-proc (if (procedure? maybe-wrap-proc)
                        maybe-wrap-proc
                        (λ(elems) (list* maybe-wrap-proc elems))))

  (define (wrap-paragraph elems)
    (if (andmap block-txexpr? elems)
        elems ; leave a series of block xexprs alone
        (list (wrap-proc elems)))) ; otherwise wrap in p tag 
  
  (let ([elements (prep-paragraph-flow elements)])
    (if (ormap explicit-or-implicit-paragraph-break? elements) ; need this condition to prevent infinite recursion
        ;; use append-map on wrap-paragraph rather than map to permit return of multiple elements
        (append-map wrap-paragraph (append-map (λ(es) (filter-split es paragraph-break?)) (slicef elements block-txexpr?))) ; split into ¶¶, using both implied and explicit paragraph breaks
        (if force-paragraph
            (append-map wrap-paragraph (slicef elements block-txexpr?)) ; upconverts non-block elements to paragraphs
            elements))))                

(module-test-external
 (check-equal? (decode-paragraphs '("First para" "\n\n" "Second para"))
               '((p "First para") (p "Second para")))
 (check-equal? (decode-paragraphs '("First para" "\n\n" "Second para" "\n" "Second line"))
               '((p "First para") (p "Second para" (br) "Second line")))
 (check-equal? (decode-paragraphs '("First para" "\n\n" (div "Second block")))
               '((p "First para") (div "Second block")))
 (check-equal? (decode-paragraphs '((div "First block") "\n\n" (div "Second block")))
               '((div "First block") (div "Second block")))
 (check-equal? (decode-paragraphs '("First para" "\n\n" "Second para") 'ns:p)
               '((ns:p "First para") (ns:p "Second para")))
 (check-equal? (decode-paragraphs '("First para" "\n\n" "Second para" "\n" "Second line")
                                  #:linebreak-proc (λ(x) (decode-linebreaks x '(newline))))
               '((p "First para") (p "Second para" (newline) "Second line")))
 (check-equal? (decode-paragraphs '("foo" "\n\n" (div "bar") (div "zam")))
               '((p "foo") (div "bar") (div "zam")))
 (check-equal? (decode-paragraphs '("foo" "\n\n" (div "bar") "\n\n" (div "zam")))
               '((p "foo") (div "bar") (div "zam")))
 
 (check-equal? (decode-paragraphs '("foo")) '("foo"))
 (check-equal? (decode-paragraphs '("foo") #:force? #t) '((p "foo")))
 (check-equal? (decode-paragraphs '((div "foo"))) '((div "foo")))
 (check-equal? (decode-paragraphs '((div "foo")) #:force? #t) '((div "foo")))
 (check-equal? (decode-paragraphs '("foo" "\n\n" (div "bar"))) '((p "foo") (div "bar")))
 (check-equal? (decode-paragraphs '("foo" (div "bar"))) '((p "foo") (div "bar")))
 (check-equal? (decode-paragraphs '("foo" (div "bar")) #:force? #t) '((p "foo") (div "bar")))
 (check-equal? (decode-paragraphs '("foo" (div "bar") "zam")) '((p "foo") (div "bar") (p "zam")))
 (check-equal? (decode-paragraphs '("foo" (span "zing") (div "bar") "zam")) '((p "foo" (span "zing")) (div "bar") (p "zam")))
 (check-equal? (decode-paragraphs '("foo" (span "zing") (div "bar") "zam") #:force? #t) '((p "foo" (span "zing")) (div "bar") (p "zam"))))

(define+provide detect-paragraphs decode-paragraphs) ; bw compat
(define+provide detect-linebreaks decode-linebreaks) ; bw compat