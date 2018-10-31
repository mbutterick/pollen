#lang racket/base
(require racket/class
         racket/gui/base
         racket/contract
         string-constants
         framework)

#|
Identical to scribble/private/indentation except it uses #\◊ rather than #\@ as the command char.

In the unit tests, `scribble/base` became `pollen/markup`
and `scribble/manual` became `pollen/markdown`
to keep the number of characters consistent, and therefore the char positions within each sample.
|#


(provide determine-spaces paragraph-indentation keystrokes)

(define paragraph-width-pref-name 'scribble-reindent-paragraph-width)
(define paragraph-width-good-val? (and/c exact-nonnegative-integer? (>=/c 10)))
(preferences:set-default paragraph-width-pref-name 60 paragraph-width-good-val?)

;; DrRacket loads this file when it encounters a suitable #lang
;; line, but the support for that doesn't correctly set up the
;; context when calling the "(λ (editor-panel) .." function, nor
;; when the callbacks of the GUI controls that that function adds.
;; So, we just disable it for now until there is time to figure out
;; a proper solution
#;
(preferences:add-to-editor-checkbox-panel
 (λ (editor-panel)
   (define hp (new horizontal-panel% [parent editor-panel] [stretchable-height #f]))
   (define tf 
     (new text-field%
          [label (string-constant reflow-paragraph-maximum-width)]
          [parent hp]
          [init-value (format "~a" (preferences:get paragraph-width-pref-name))]
          [callback
           (λ (x y)
             (update-pref)
             (update-tf-bkg))]))
   (define (update-tf-bkg)
     (send tf set-field-background
           (send the-color-database find-color 
                 (cond
                   [(paragraph-width-good-val? (string->number (send tf get-value)))
                    "white"]
                   [else
                    "yellow"]))))
   (define (update-pref)
     (define current (preferences:get paragraph-width-pref-name))
     (define candidate-num (string->number (send tf get-value)))
     (when (paragraph-width-good-val? candidate-num)
       (preferences:set paragraph-width-pref-name candidate-num)))
   (update-tf-bkg)))

(define (reindent-paragraph t evt)
  (unless (send t is-stopped?)
    (define sp (send t get-start-position))
    (when (= sp (send t get-end-position))
      (paragraph-indentation
       t sp
       (preferences:get paragraph-width-pref-name)))))

(define keystrokes
  (append (list (list "esc;q" reindent-paragraph)
                (list "?:a:q" reindent-paragraph))
          (if (equal? (system-type) 'unix)
              (list (list "m:q" reindent-paragraph))
              (list))))

;;(paragraph-indentation a-racket:text posi width) → void?
;; pos : exact-integer? = current given position
;; width : exact-integer? = user defined line width limitation
;; Indent a whole paragraph (multiple lines that contains the given position)
(define (paragraph-indentation txt pos width)
  (define pos-para (send txt position-paragraph pos))

  (wait-for-colorer txt pos)

  (define-values (start-position end-position)
    (find-paragraph-boundaries txt pos))
  (when (and start-position end-position)
    (send txt begin-edit-sequence)
    (join-paragraphs txt start-position end-position)
    (define new-end-position (compress-whitespace txt start-position end-position))
    (define new-new-end-position (break-paragraphs txt start-position new-end-position width))

    ;; reindent at newly inserted newlines
    (let ([end-para (send txt position-paragraph new-new-end-position)])
      (let loop ([para (send txt position-paragraph start-position)])
        (when (<= para end-para)
          (define sp (send txt paragraph-start-position para))
          (define ep (send txt paragraph-end-position para))
          (define s (determine-spaces txt sp))
          (when s
            (define to-delete
              (let loop ([pos sp]
                         [amt 0])
                (cond
                  [(= pos ep) amt]
                  [(char-whitespace? (send txt get-character pos))
                   (loop (+ pos 1) (+ amt 1))]
                  [else amt])))
            (send txt delete sp (+ sp to-delete))
            (send txt insert (make-string s #\space) sp sp))
          (loop (+ para 1)))))
    
    (send txt end-edit-sequence)))

;; do this to ensure the colorer is sync'd, since
;; classify position returns bogus results if it isn't
;; this should ensure the colorer is sync up to `pos`
(define (wait-for-colorer txt pos)
  (let ([bw (send txt backward-containing-sexp pos 0)])
    (send txt forward-match (or bw pos) (send txt last-position))))

(define (find-paragraph-boundaries txt insertion-pos)

  ;; move back one position in the case that
  ;; we are at the end of the buffer or we are
  ;; at the end of a text region
  (define insertion-pos/one-earlier
    (cond
      [(= insertion-pos (send txt last-position))
       (- insertion-pos 1)]
      [(and (equal? (send txt classify-position insertion-pos)
                    'parenthesis)
            (> insertion-pos 0)
            (is-text? txt (- insertion-pos 1)))
       (- insertion-pos 1)]
      [else insertion-pos]))
  

  ;; find a starting point that's in a 'text' region
  ;; and in the same paragraph as the insertion-pos/one-earlier
  (define pos
    (let loop ([pos insertion-pos/one-earlier])
      (cond
        [(is-text? txt pos)
         pos]
        [else
         (define containing-start (send txt find-up-sexp pos))
         (define pos-para (send txt position-paragraph pos))
         (cond
           [(not containing-start)
            ;; we know there is no sexp outside us here
            (wait-for-colorer txt (min (+ pos 1) (send txt last-position)))
            (cond
              [(and (= pos (send txt paragraph-end-position pos-para))
                    (begin
                      ;; when we are the end of a paragraph, it might be "morally"
                      ;; text, but the colorerer can't tell us that without a
                      ;; character actually being there to classify
                      ;; so add one and check it
                      (send txt insert " " pos pos)
                      (wait-for-colorer txt pos)
                      (begin0
                        (is-text? txt pos)
                        (send txt delete pos (+ pos 1)))))
               pos]
              [else
               ;; at this point, we might be at the end of the word
               ;; `hello` in something like `◊hello[there]`
               ;; so scan backwards to see if the first paren we find
               ;; is an `◊` and, in that case, go one before it and try again
               (let loop ([pos (if (is-open? txt pos)
                                   (- pos 1)
                                   pos)])
                 (define cp (send txt classify-position pos))
                 (cond
                   [(not (= (send txt position-paragraph pos) pos-para))
                    #f]
                   [(member cp '(symbol keyword))
                    (if (= pos 0)
                        #f
                        (loop (- pos 1)))]
                   [(equal? cp 'parenthesis)
                    (if (and (> pos 0)
                             (is-text? txt (- pos 1)))
                        (- pos 1)
                        #f)]
                   [else #f]))])]
           [(= (send txt position-paragraph containing-start) pos-para)
            (loop containing-start)]
           [else
            (define containing-end
              (send txt forward-match containing-start (send txt last-position)))
            (cond
              [(not containing-end) #f]
              [(= (send txt position-paragraph containing-end) pos-para)
               (loop containing-end)]
              [else #f])])])))
  
  (cond
    [pos

     ;; find limits on how far we will reflow the paragraph.
     ;; we definitely won't go beyond a blank line, but maybe
     ;; we have to stop before we find a blank line. This code
     ;; finds the spot we should stop at by looking at the sexp
     ;; structure of the program text.
     ;;
     ;; we are looking for an enclosing sexp that's in "racket"
     ;; mode, i.e. something like ◊f[(f x ◊emph{no, really!})]
     ;; if we start inside the `no really!` part, then we don't
     ;; want to reflow past that call to `f`.
     ;;
     ;; #f means no limit
     (define-values (start-sexp-boundary end-sexp-boundary)
       (let ([first-container (send txt find-up-sexp pos)])
         (cond
           [first-container
            (define start-sexp-boundary
              (let loop ([pos pos])
                (define container (send txt find-up-sexp pos))
                (cond
                  [container
                   (define paren (send txt get-character container))
                   (cond
                     [(and (equal? paren #\{) (not (= pos 0)))
                      (loop container)]
                     [else pos])]
                  [else pos])))
            (values start-sexp-boundary
                    (send txt forward-match start-sexp-boundary
                          (send txt last-position)))]
           [else
            (values #f #f)])))

     ;; once we know the sexp-based limits, we look for any blank lines that
     ;; might cause us to stop earlier
     (define start-sexp-para
       (send txt position-paragraph (or start-sexp-boundary 0)))
     (define end-sexp-para
       (send txt position-paragraph
             (cond
               [end-sexp-boundary end-sexp-boundary]
               [else
                (define lp (send txt last-position))
                (if (and (0 . < . lp)
                         (equal? #\newline (send txt get-character (- lp 1))))
                    (- lp 1)
                    lp)])))

     (cond
       [(or (and start-sexp-boundary (empty-para? txt start-sexp-para))
            (and end-sexp-boundary (empty-para? txt end-sexp-para)))
        ;; this shouldn't be possible (I think?) but be conservative in case it is
        (values #f #f)]
       [else
        (define start-position
          (let loop ([para (send txt position-paragraph pos)])
            (cond
              [(= start-sexp-para para)
               (or start-sexp-boundary (send txt paragraph-start-position para))]
              [(empty-para? txt para)
               (send txt paragraph-start-position (+ para 1))]
              [else
               (loop (- para 1))])))
        (define end-position
          (let loop ([para (send txt position-paragraph pos)])
            (cond
              [(= para end-sexp-para)
               (or end-sexp-boundary (send txt paragraph-end-position para))]
              [(empty-para? txt para)
               (send txt paragraph-end-position (- para 1))]
              [else
               (loop (+ para 1))])))
        (values start-position end-position)])]
    [else (values #f #f)]))

(define (is-open? txt pos)
  (define cp (send txt classify-position pos))
  (and (equal? cp 'parenthesis)
       (member (send txt get-character pos) '(#\( #\{ #\[))))

(define (empty-para? txt para)
  (for/and ([x (in-range (send txt paragraph-start-position para)
                         (send txt paragraph-end-position para))])
    (char-whitespace? (send txt get-character x))))

;; note: this might change the number of characters in the text, if
;; it chooses to break right after a {; the result accounts for that.
(define (break-paragraphs txt start-position end-position width)
  (define δ 0)

  (define (break-line pos is-whitespace?)
    (cond
      [is-whitespace?
       (send txt delete pos (+ pos 1))]
      [else
       (set! δ (+ δ 1))])
    (send txt insert "\n" pos pos))

  (let para-loop ([para (send txt position-paragraph start-position)]
                  [first-legal-pos-in-para start-position])
    (define para-start (send txt paragraph-start-position para))
    (let char-loop ([pos (or first-legal-pos-in-para para-start)]
                    [previous-candidate #f]
                    [previous-candidate-is-whitespace? #f])
      (cond
        [(and previous-candidate (> (- pos para-start) width))
         (break-line previous-candidate previous-candidate-is-whitespace?)
         (para-loop (+ para 1) #f)]
        [(= pos end-position)
         (when (equal? previous-candidate (- pos 1))
           (send txt delete (- pos 1) pos))]
        [else
         (define is-whitespace? (char-whitespace? (send txt get-character pos)))
         (define linebreak-candidate?
           (and (is-text? txt pos)
                (or is-whitespace?
                    (and (pos . > . 0)
                         (equal? 'parenthesis (send txt classify-position (- pos 1)))
                         (equal? #\{ (send txt get-character (- pos 1)))))))
         (cond
           [(and linebreak-candidate? (> (- pos para-start) width))
            (break-line pos is-whitespace?)
            (para-loop (+ para 1) #f)]
           [else
            (char-loop (+ pos 1)
                       (if linebreak-candidate?
                           pos
                           previous-candidate)
                       (if linebreak-candidate?
                           is-whitespace?
                           previous-candidate-is-whitespace?))])])))
  (+ end-position δ))

;; the colorer classifies nearly all text as 'text but
;; some whitespace that's in a text region is
;; classified as 'white-space instead, so search backwards
;; for either text or a { when we find 'white-space
(define (is-text? txt pos)
  (define classified (send txt classify-position pos))
  (or (equal? classified 'text)
      (and (equal? classified 'white-space)
           (let ([backward (send txt find-up-sexp pos)])
             (and backward
                  (equal? (send txt get-character backward) #\{)
                  (equal? (send txt classify-position backward)
                          'parenthesis))))))
           

;; invariant: does not change the the where the positions are in the editor
;; (except temporarily between the delete and insert)
(define (join-paragraphs txt start-position end-position)
  (define start-para (send txt position-paragraph start-position))
  (define end-para (send txt position-paragraph end-position))
  (let loop ([para end-para])
    (unless (= para start-para)
      (define start (send txt paragraph-start-position para))
      (send txt delete (- start 1) start)
      (send txt insert " " (- start 1) (- start 1))
      (loop (- para 1)))))

(define (compress-whitespace txt start-position end-position)
  (let loop ([pos start-position]
             [end-position end-position]
             [last-whitespace? #f])
    (cond
      [(< pos end-position)
       (define char (send txt get-character pos))
       (define this-whitespace? (char-whitespace? char))
       (when (and this-whitespace?
                  (not last-whitespace?)
                  (not (char=? #\space char)))
         (send txt delete pos (+ pos 1))
         (send txt insert " " pos))
       (cond
         [(and last-whitespace?
               this-whitespace?
               (is-text? txt pos))
          (send txt delete pos (+ pos 1))
          (loop pos (- end-position 1) #t)]
         [else
          (loop (+ pos 1) end-position this-whitespace?)])]
      [else end-position])))

;;(rest-empty? a-racket:text line start) → boolean?
;; line : exact-integer? = (send text position-paragraph start)
;; start : exact-intefer? = the start position
(define (rest-empty? txt line start)
  (let* ([line-start (add1 start)]
         [line-end (send txt paragraph-end-position line)]
         [line-classify (txt-position-classify txt line-start line-end)])
    (not (para-not-empty? line-classify))))

;;(determine-spaces : a-racket:text position) → exact-integer?/boolean?
;; position : exact-integer? = current position
;; Return exact integer represents number of #\space to put in front of current paragraph(line) or #f
(define (determine-spaces txt posi)
  (define current-para (send txt position-paragraph posi))
  (define para-start (send txt paragraph-start-position current-para))
  (define para-start-skip-space (start-skip-spaces txt current-para 'forward))
  (cond
    [para-start-skip-space
     (define char-classify (send txt classify-position para-start-skip-space))
     (define prev-posi (send txt find-up-sexp para-start-skip-space))
     (cond
       [prev-posi
        (define this-para (send txt position-paragraph prev-posi))
        (cond
          [(equal? #\[ (send txt get-character prev-posi))
           (define this-para-start (send txt paragraph-start-position this-para))
           (if (= current-para this-para)
               0
               (if (rest-empty? txt this-para prev-posi)
                   1
                   (add1 (- prev-posi this-para-start))))]
          
          ;;if it is inside a racket function and not the first line of the racket function
          [(equal? #\( (send txt get-character prev-posi)) #f]
          [else
           (define curleys (number-of-curley-braces-if-there-are-only-curley-braces txt current-para))
           (if curleys
               (max 0 (- (count-parens txt prev-posi) curleys))
               (count-parens txt prev-posi))])]
       [(equal? 'text char-classify) 0]
       [else #f])]
    [else #f]))

(define (number-of-curley-braces-if-there-are-only-curley-braces txt para)
  (define number-of-curley-braces 0)
  (define line-contains-only-curley-braces?
    (for/and ([p (in-range (send txt paragraph-start-position para)
                           (send txt paragraph-end-position para))])
      (define c (send txt get-character p))
      (cond
        [(char-whitespace? c) #t]
        [(equal? c #\})
         (set! number-of-curley-braces (+ number-of-curley-braces 1))
         #t]
        [else #f])))
  (and line-contains-only-curley-braces?
       number-of-curley-braces))

;;(txt-position-classify a-racket:text start end) → void?
;; start : exact-integer? = position to start classify
;; end : exact-integer? = position to end classify
;; Basic position classify method that classify text within certain range
(define (txt-position-classify txt start end)
  (for/list ([x (in-range start end 1)])
    (send txt classify-position x)))

;;(is-at-sign? a-racket:text posi) → boolean?
;; posi : exact-integer? = a position in the text
;; Check if the given position is an ◊

;; formerly called `is-at-sign?`
(define (is-pollen-lozenge? txt posi)
  (and (equal? (send txt classify-position posi) 'parenthesis)
       (let-values ([(start end) (send txt get-token-range posi)])
         (and (equal? start posi)
              (equal? end (+ posi 1))))
       (equal? #\◊ (send txt get-character posi))))

;;(para-not-empty? a-racket:text classify-lst) → boolean?
;; classify-lst : list? = (txt-position-classify text start end)
;; Check if current paragraph(line) is empty, we consider comment line as empty line
(define (para-not-empty? classify-lst) ;;we consider 'other  and 'comment as empty
  (and (or (member 'parenthesis classify-lst)
           (member 'string classify-lst)
           (member 'symbol classify-lst)
           (member 'text classify-lst))
       #t))

;;(start-skip-spaces a-racket:text para direction) → exact-integer?
;;para : exact-integer? = paragraph(line) number
;;direction : symbol? = 'forward/'backward
;;Return the first non-empty(#\space #\tab) character's position in given paragraph(line)
(define (start-skip-spaces txt para direction)
  (let* ([para-start (send txt paragraph-start-position para)]
         [para-end (send txt paragraph-end-position para)])
    (if (equal? direction 'forward)
        (for/first ([start-skip-space (in-range para-start para-end 1)]
                    #:unless (member (send txt get-character start-skip-space) (list #\space #\tab)))
          start-skip-space)
        (for/first ([start-skip-space (in-range (sub1 para-end) para-start -1)];;ignore the newline
                    #:unless (member (send txt get-character start-skip-space)  (list #\space #\tab)))
          start-skip-space))))

;;(delete-end-spaces a-racket:text para) → void?
;;para : exact-integer? = paragraph(line) number
;;Delete all #\space and #\tab at the end of given paragraph
(define (delete-end-spaces txt para)
  (let* ([para-end (send txt paragraph-end-position para)]
         [last-non-white (start-skip-spaces txt para 'backward)])
    (if last-non-white
        (send txt delete (+ last-non-white 1) para-end)
        #f)))

;;(delete-start-spaces a-racket:text para) → void?
;;para : exact-integer? = paragraph(line) number
;;Delete all #\space and #\tab at the beginning of given paragraph
(define (delete-start-spaces txt para)
  (let* ([para-start (send txt paragraph-start-position para)]
         [first-non-white (start-skip-spaces txt para 'forward)])
    (when (and first-non-white (> first-non-white para-start))
      (send txt delete para-start first-non-white))))

;;(count-parens a-racket:text posi) → exact-integer?
;;posi : exact-integer? = a position in given text 
;;Return number of parenthesis till the outmost ◊ annotation,
;;if the there is "[", we check if it has "◊" after at the same
;;line, if so, we add the number of characters between "[" and
;;the beginning of the line it appears
(define (count-parens txt posi)
  (define count 0)
  (do ([p posi (send txt find-up-sexp p)]);backward-containing-sexp p 0)])
    ((not p) count)
    (cond [(equal? #\{ (send txt get-character p)) (set! count (add1 count))]
          [(equal? #\[ (send txt get-character p))
           (let* ([this-para (send txt position-paragraph p)]
                  [this-para-start (send txt paragraph-start-position this-para)])
             (if (rest-empty? txt this-para p)
                 (set! count (add1 count))
                 (set! count (+ (add1 (- p this-para-start)) count))))]
          [else #t])))

;;(insert-break-text a-racket:text start width-end end) → exact-integer?/boolean?
;; start : exact-integer? = (send text paragraph-start-position given-paragraph-number)
;; width-end : exact-integer? = (+ start width) here width is the user defined line width limit
;; end : exact-integer? = (send text paragraph-end-position given-paragraph-number)
;;Return the proper position to insert #\newline into given text line, #f if not found
(define (insert-break-text text start width-end end)
  (for/first ([break-ls (in-range width-end start -1)]
              #:when (equal? #\space (send text get-character break-ls)))
    break-ls))

;;(insert-break-func a-racket:text start len width classify-lst) → exact-integer?/boolean?
;; start : exact-integer? = (send text paragraph-start-position given-paragraph-number)
;; len : exact-integer? = length of current paragraph(line)
;; width : exact-integer? = predefined value
;; classify-lst: list? = (txt-position-classify text start end) here end is the end position
;;               of current paragraph(line)
;;Return the proper position to insert #\newline into given line, #f if not found
(define (insert-break-func text start len width classify-lst)
  (let ([at-sign-posi
         (for/first ([sign-posi (in-range (+ start width) start -1)]
                     #:when (is-pollen-lozenge? text sign-posi))
           sign-posi)])
    (if (and at-sign-posi
             (not (equal? 'white-space (list-ref classify-lst (sub1 (- at-sign-posi start))))))
        at-sign-posi
        (for/first ([posi (in-range width (- len 1))] 
                    #:when (equal? 'text (list-ref classify-lst posi)))
          (+ start posi)))))

;;adjust-spaces for text
;;(adjust-spaces : a-racket:text para amount posi) → boolean?
;; para : exact-integer? = given paragraph(line) number
;; amount : exact-integer? = (determine-spaces text posi)
;; posi : exact-integer? = a position in the text
;; Delete #\spaces and #\tab in front of given paragraph(line) if not 
;;  equal to the amount given by determine-spaces. Then insert new amount of #\space
;;  in front of the paragraph(line)
(define (adjust-spaces text para amount posi)
  (define posi-skip-space (start-skip-spaces text para 'forward))
  (when posi-skip-space
    (define origin-amount (- posi-skip-space posi))
    (when (and amount (not (= origin-amount amount)))
      (send text delete posi posi-skip-space)
      (when (> amount 0)
        (send text insert (make-string amount #\space) posi))))
  #t)



(define/contract (insert-them t . strs)
  (->* ((is-a?/c text%)) #:rest (cons/c (and/c string? #rx"\n$") (listof string?)) void?)
  (for ([str (in-list strs)])
    (define lp (send t last-position))
    (send t insert str lp lp))
  (send t freeze-colorer)
  (send t thaw-colorer))

#;(module+ test
  (require rackunit framework)
  
  ;test start-skip-spaces
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "test1 test2\n ◊test3\n")
                  (start-skip-spaces t 1 'forward)) 13)
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abcd\n◊efgh\n}")
                  (start-skip-spaces t 1 'forward)) 6)
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abcd\n  efgh\n}")
                  (start-skip-spaces t 1 'forward)) 8)
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abc\n       \n}")
                  (start-skip-spaces t 1 'forward)) #f)
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abc\nefgh   \n}")
                  (start-skip-spaces t 1 'backward)) 8)
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abcd\n\t\tefgh\n}");tab
                  (start-skip-spaces t 1 'forward)) 8)
  (define txt_1 (new racket:text%))
  (send txt_1 insert "#lang pollen/markup\n◊f{x}\n◊;ghj\ntyty\n\n")
  
 ;test is-at-sign
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "(x)")
                  (is-pollen-lozenge? t 0))
                #f)
  (check-equal? (is-pollen-lozenge? txt_1 20) #t)
  (check-equal? (is-pollen-lozenge? txt_1 22) #f) 
  
  ;test determine-spaces
  (check-equal? (determine-spaces txt_1 15) #f)
  (check-equal? (determine-spaces txt_1 21) #f)
  
  (define txt_2 (new racket:text%))
  (send txt_2 insert "#lang pollen/markup\n◊f{\n ◊a\n◊b\n}")
  (check-equal? (determine-spaces txt_2 25) 1)
  (check-equal? (determine-spaces txt_2 28) 1)
  
  (define txt_3 (new racket:text%))
  (send txt_3 insert "#lang pollen/markup\n◊f[◊x\n◊y\n]")
  (check-equal? (determine-spaces txt_3 24) #f) 
  (check-equal? (determine-spaces txt_3 27) 3)
  
  (define txt_4 (new racket:text%))
  (send txt_4 insert "#lang pollen/markup\n◊itemlist[◊item{item1}\n◊item{item2}\n]")
  (check-equal? (determine-spaces txt_4 22) #f)
  (check-equal? (determine-spaces txt_4 44) 10)
  
  (define txt_5 (new racket:text%))
  (send txt_5 insert "#lang pollen/markup\n◊boldlist{◊me{item1}\n◊me{item2}\n}")
  (check-equal? (determine-spaces txt_5 31) #f)
  (check-equal? (determine-spaces txt_5 46) 1)
  
  (define txt_6 (new racket:text%))
  (send txt_6 insert "◊list{◊me{item1}\n\n◊me{item2}\n}")
  (check-equal? (determine-spaces txt_6 16) #f)
  (check-equal? (determine-spaces txt_6 17) #f);empty line!
  (check-equal? (determine-spaces txt_6 18) 1)
  
  (check-equal? (let ([txt_7 (new racket:text%)])
                  (send txt_7 insert "◊(define (foo . a)\n(bar b))")
                  (determine-spaces txt_7 19)) #f)
  
  (define txt_8 (new racket:text%))
  (send txt_8 insert "◊a{me}\n◊b[\n◊c{◊d{e} f\ng\nh}\n")
  (check-equal? (count-parens txt_8 22) 2)
  (check-equal? (count-parens txt_8 13) 2);;include current parenthesis
  (check-equal? (determine-spaces txt_8 22) 2)
  (check-equal? (determine-spaces txt_8 12) 1) 
  
  (define txt_9 (new racket:text%))
  (send txt_9 insert "◊a[\n(b c)\n(d\n[(e) f]\n[g h])\n]\n")
  (check-equal? (determine-spaces txt_9 13) #f) 
  (check-equal? (determine-spaces txt_9 4) 1)
  
  ;;two test cases for count-parens
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "◊a[◊b{c d\ne f g}\n◊h{i j}]")
                  (count-parens t 5)) 4)
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "◊a[◊b{\n◊c{d e f}\ng}]")
                  (count-parens t 9)) 5)

  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "◊a[\n     ]\n")
                  (determine-spaces t 4))
                1)      
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang pollen/markup\n\ntest1\n     test2\n")
                  (determine-spaces t 28))
                0)
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang pollen/markup\n\ntestcase ◊a{b\n\n\n\n\n      c}\n\n")
                  (determine-spaces t 39))
                1)
  ;;test cases for:delete-end-spaces delete-start-spaces
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abcde   \nfgh\n}")
                  (delete-end-spaces t 0)
                  (send t get-text)) "{abcde\nfgh\n}")
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abcde\t\t\t\t\nfgh\n}")
                  (delete-end-spaces t 0)
                  (send t get-text)) "{abcde\nfgh\n}")
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "{abcde   \n\n3\n}")
                  (delete-end-spaces t 0)
                  (send t get-text)) "{abcde\n\n3\n}")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "  {abcde\nfgh\n}")
                  (delete-start-spaces t 0)
                  (send t get-text)) "{abcde\nfgh\n}")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "◊a[\n      ]\n")
                  (delete-start-spaces t 1)
                  (send t get-text)) "◊a[\n]\n")
  
  ;;adjust-spaces
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "◊a[\n     ]\n")
                  (adjust-spaces t 1 1 4)
                  (adjust-spaces t 1 1 4)
                  (send t get-text)) "◊a[\n ]\n")
  

 
;                                                                        
;                                                                        
;                                                                        
;                                                                        
;                                                            ;;;         
;                                                            ;;;         
;  ;;; ;;   ;;;;;  ;;; ;;;;;;;   ;; ;;; ;;; ;;;;;;;  ;;; ;;  ;;; ;;      
;  ;;;;;;; ;;;;;;; ;;;;;;;;;;;; ;;;;;;; ;;;;;;;;;;;; ;;;;;;; ;;;;;;;     
;  ;;; ;;; ;;  ;;; ;;;  ;;  ;;; ;;; ;;; ;;;  ;;  ;;; ;;; ;;; ;;; ;;;     
;  ;;; ;;;   ;;;;; ;;;    ;;;;; ;;; ;;; ;;;    ;;;;; ;;; ;;; ;;; ;;; ;;;;
;  ;;; ;;; ;;; ;;; ;;;  ;;; ;;; ;;; ;;; ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;;;
;  ;;;;;;; ;;; ;;; ;;;  ;;; ;;; ;;;;;;; ;;;  ;;; ;;; ;;;;;;; ;;; ;;;     
;  ;;; ;;   ;;;;;; ;;;   ;;;;;;  ;; ;;; ;;;   ;;;;;; ;;; ;;  ;;; ;;;     
;  ;;;                              ;;;              ;;;                 
;  ;;;                          ;;;;;;               ;;;                 
;                                                                        
;                                                                        
;                                                                            
;                                                                            
;                                                                            
;                                                                            
;  ;;;             ;;;                   ;            ;  ;;;                 
;                  ;;;                 ;;;          ;;;                      
;  ;;; ;;; ;;   ;; ;;;   ;;;;  ;;; ;;  ;;;;  ;;;;;  ;;;; ;;;   ;;;   ;;; ;;  
;  ;;; ;;;;;;; ;;;;;;;  ;; ;;; ;;;;;;; ;;;; ;;;;;;; ;;;; ;;;  ;;;;;  ;;;;;;; 
;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ;;  ;;; ;;;  ;;; ;;; ;;; ;;; ;;; 
;  ;;; ;;; ;;; ;;; ;;; ;;;;;;; ;;; ;;; ;;;    ;;;;; ;;;  ;;; ;;; ;;; ;;; ;;; 
;  ;;; ;;; ;;; ;;; ;;; ;;;     ;;; ;;; ;;;  ;;; ;;; ;;;  ;;; ;;; ;;; ;;; ;;; 
;  ;;; ;;; ;;; ;;;;;;;  ;;;;;; ;;; ;;; ;;;; ;;; ;;; ;;;; ;;;  ;;;;;  ;;; ;;; 
;  ;;; ;;; ;;;  ;; ;;;   ;;;;  ;;; ;;;  ;;;  ;;;;;;  ;;; ;;;   ;;;   ;;; ;;; 
;                                                                            
;                                                                            
;                                                                            
;                                                                            

  (let ([t (new racket:text%)])
    (insert-them t
                 "#lang pollen/markup\n"
                 "\n"
                 "◊f{x}")
    (check-equal? (is-text? t 21) #f)
    (check-equal? (is-text? t 22) #f)
    (check-equal? (is-text? t 23) #f)
    (check-equal? (is-text? t 24) #t)
    (check-equal? (is-text? t 25) #f))

  (let ([t (new racket:text%)])
    (insert-them t
                 "#lang pollen/markup\n"
                 "\n"
                 "◊f{ x }")
    (check-equal? (is-text? t 21) #f)
    (check-equal? (is-text? t 22) #f)
    (check-equal? (is-text? t 23) #f)
    (check-equal? (is-text? t 24) #t)
    (check-equal? (is-text? t 25) #t)
    (check-equal? (is-text? t 26) #t)
    (check-equal? (is-text? t 27) #f))

  (let ([t (new racket:text%)])
    (insert-them t
                 "#lang pollen/markup\n"
                 "\n"
                 "◊f{\n\n\n}")
    (check-equal? (is-text? t 21) #f)
    (check-equal? (is-text? t 22) #f)
    (check-equal? (is-text? t 23) #f)
    (check-equal? (is-text? t 24) #t)
    (check-equal? (is-text? t 25) #t)
    (check-equal? (is-text? t 26) #t)
    (check-equal? (is-text? t 27) #f))

  (let ([t (new racket:text%)])
    (insert-them t
                 "#lang pollen/markup\n"
                 "\n"
                 "◊f{\nx\n}")
    (check-equal? (is-text? t 21) #f)
    (check-equal? (is-text? t 22) #f)
    (check-equal? (is-text? t 23) #f)
    (check-equal? (is-text? t 24) #t)
    (check-equal? (is-text? t 25) #t)
    (check-equal? (is-text? t 26) #t)
    (check-equal? (is-text? t 27) #f))

  (let ([t (new racket:text%)])
    (insert-them t
                 "#lang pollen/markup\n"
                 "\n"
                 "◊f{\n \n}")
    (check-equal? (is-text? t 21) #f)
    (check-equal? (is-text? t 22) #f)
    (check-equal? (is-text? t 23) #f)
    (check-equal? (is-text? t 24) #t)
    (check-equal? (is-text? t 25) #t)
    (check-equal? (is-text? t 26) #t)
    (check-equal? (is-text? t 27) #f))

  (let ([t (new racket:text%)])
    (insert-them t
                 "#lang pollen/markup\n"
                 "\n"
                 "aaa bbb ccc\n"
                 "  ◊ddd[eee] fff\n"
                 " ggg hhh iii jjj\n")
    (check-equal? (call-with-values (λ () (find-paragraph-boundaries t 23))
                                    list)
                  (list 21 65)))

  (let ([t (new racket:text%)])
    (insert-them t
                 "#lang pollen/markup\n"
                 "\n"
                 "aaa bbb ccc\n"
                 "  ◊ddd[eee] fff\n"
                 " ggg hhh iii jjj")
    (check-equal? (call-with-values (λ () (find-paragraph-boundaries t 23))
                                    list)
                  (list 21 65)))

  (let ([t (new racket:text%)])
    (insert-them t
                 "#lang pollen/markup\n"
                 "\n"
                 "◊itemlist[◊item{aaa bbb ccc\n"
                 "                eee fff}\n"
                 "          ◊item{ggg hhh iii\n"
                 "  jjj kkk lll mmm nnn ooo\n"
                 "  ppp qqq\n"
                 "rrr\n"
                 "sss ttt uuu vvv}]\n")
    (check-equal? (call-with-values (λ () (find-paragraph-boundaries t 38)) list)
                  (list 36 73)))
  

  (let ([t (new racket:text%)])
    (send t insert "#lang pollen/markup\n")
    (for ([x (in-range 6)])
      (send t insert "a " (send t last-position) (send t last-position)))
    (break-paragraphs t (send t paragraph-start-position 1) (send t last-position) 4)
    (check-equal? (send t get-text)
                  (string-append
                   "#lang pollen/markup\n"
                   "a a\n"
                   "a a\n"
                   "a a")))

  (let ([t (new racket:text%)])
    (send t insert "#lang pollen/markup\n")
    (for ([x (in-range 6)])
      (send t insert "a " (send t last-position) (send t last-position)))
    (break-paragraphs t (send t paragraph-start-position 1) (send t last-position) 8)
    (check-equal? (send t get-text)
                  (string-append
                   "#lang pollen/markup\n"
                   "a a a a\n"
                   "a a")))
  
  (let ([t (new racket:text%)])
    (send t insert "#lang pollen/markup\n")
    (for ([x (in-range 30)])
      (send t insert "a " (send t last-position) (send t last-position)))
    (break-paragraphs t (send t paragraph-start-position 1) (send t last-position) 10)
    (check-equal? (send t get-text)
                  (string-append "#lang pollen/markup\n"
                                 "a a a a a\n"
                                 "a a a a a\n"
                                 "a a a a a\n"
                                 "a a a a a\n"
                                 "a a a a a\n"
                                 "a a a a a")))

  (let ([t (new racket:text%)])
    (insert-them t
                 "#lang pollen/markup\n"
                 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n")
    (break-paragraphs t (send t paragraph-start-position 1) (send t last-position) 10)
    (check-equal? (send t get-text)
                  (string-append "#lang pollen/markup\n"
                                 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n"
                                 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n")))

  (let ([t (new racket:text%)])
    (insert-them t
                 "#lang pollen/markup\n"
                 "aa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n")
    (break-paragraphs t (send t paragraph-start-position 1) (send t last-position) 10)
    (check-equal? (send t get-text)
                  (string-append "#lang pollen/markup\n"
                                 "aa\n"
                                 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n"
                                 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n")))

  (check-equal? (let ([t (new racket:text%)])
                  (list (compress-whitespace t 0 (send t last-position))
                        (send t get-text)))
                '(0 ""))
  (let ([t (new racket:text%)])
    (insert-them t
                 "#lang pollen/markup\n"
                 "abcdef")
    (check-equal?   (list (compress-whitespace t
                                               (send t paragraph-start-position 1)
                                               (send t last-position))
                          (send t get-text))
                    (list (send t paragraph-end-position 1)
                          "#lang pollen/markup\nabcdef")))
  (let ([t (new racket:text%)])
    (insert-them t
                 "#lang pollen/markup\n"
                 "a cd f")
    (check-equal? 
     (list (compress-whitespace t
                                (send t paragraph-start-position 1)
                                (send t last-position))
           (send t get-text))
     (list (send t paragraph-end-position 1)
           "#lang pollen/markup\na cd f")))
  (let ([t (new racket:text%)])
    (insert-them t
                 "#lang pollen/markup\n"
                 "a    f")
    (check-equal? 
     (list (compress-whitespace t
                                (send t paragraph-start-position 1)
                                (send t last-position))
           (send t get-text))
     (list (+ (send t paragraph-start-position 1) 3)
           "#lang pollen/markup\na f")))
  (let ([t (new racket:text%)])
    (insert-them t
                 "#lang pollen/markup\n"
                 "a    f")
    (check-equal? 
     (list (compress-whitespace t
                                (send t paragraph-start-position 1)
                                (- (send t last-position) 1))
           (send t get-text))
     (list (+ (send t paragraph-start-position 1) 2)
           "#lang pollen/markup\na f")))
  (let ([t (new racket:text%)])
                  (insert-them t
                               "#lang pollen/markup\n"
                               "a    f")
    (check-equal? (list (compress-whitespace t
                                             (send t paragraph-start-position 1)
                                             (- (send t last-position) 2))
                        (send t get-text))
                  (list (+ (send t paragraph-start-position 1) 2)
                        "#lang pollen/markup\na  f")))

  (check-equal? (let ([t (new racket:text%)])
                  (insert-them t
                               "#lang pollen/markup\n"
                               "\n"
                               "aaa bbb ccc\n"
                               "  ◊ddd[eee] fff\n"
                               " ggg hhh iii jjj\n")
                  (paragraph-indentation t 23 23)
                  (send t get-text))
                (string-append
                 "#lang pollen/markup\n"
                 "\n"
                 "aaa bbb ccc ◊ddd[eee]\n"
                 "fff ggg hhh iii jjj\n"))

  (check-equal? (let ([t (new racket:text%)])
                  (insert-them t
                               "#lang pollen/markup\n"
                               "\n"
                               "aaa bbb ccc\n"
                               "  ◊ddd[eee] fff\n"
                               " ggg hhh iii jjj\n\n\n")
                  (paragraph-indentation t 23 23)
                  (send t get-text))
                (string-append
                 "#lang pollen/markup\n"
                 "\n"
                 "aaa bbb ccc ◊ddd[eee]\n"
                 "fff ggg hhh iii jjj\n\n\n"))
  
  (check-equal? (let ([t (new racket:text%)])
                  (insert-them t
                               "#lang pollen/markup\n"
                               "\n"
                               "◊itemlist[◊item{aaa bbb ccc\n"
                               "                eee fff}\n"
                               "          ◊item{ggg hhh iii\n"
                               "  jjj kkk lll mmm nnn ooo\n"
                               "  ppp qqq\n"
                               "rrr\n"
                               "sss ttt uuu vvv}]\n")
                  (paragraph-indentation t 38 29)
                  (send t get-text))
                (string-append
                 "#lang pollen/markup\n"
                 "\n"
                 "◊itemlist[◊item{aaa bbb ccc\n"
                 "           eee fff}\n"
                 "          ◊item{ggg hhh iii\n"
                 "  jjj kkk lll mmm nnn ooo\n"
                 "  ppp qqq\n"
                 "rrr\n"
                 "sss ttt uuu vvv}]\n"))
  
  (check-equal? (let ([t (new racket:text%)])
                  (insert-them t
                               "#lang pollen/markup\n"
                               "\n"
                               "◊itemlist[◊item{aaa bbb ccc\n"
                               "                eee fff\n"
                               "          ◊item{ggg hhh iii\n"
                               "  jjj kkk lll mmm nnn ooo\n"
                               "  ppp qqq\n"
                               "rrr\n"
                               "sss ttt uuu vvv}}]\n")
                  (paragraph-indentation t 38 29)
                  (send t get-text))
                (string-append
                 "#lang pollen/markup\n"
                 "\n"
                 "◊itemlist[◊item{aaa bbb ccc\n"
                 "           eee fff ◊item{ggg hhh iii jjj\n"
                 "            kkk lll mmm nnn ooo ppp qqq\n"
                 "            rrr sss ttt uuu vvv}}]\n"))

  (check-equal? (let ([t (new racket:text%)])
                  (insert-them t
                               "#lang pollen/markup\n"
                               "\n"
                               "jflkda fkfjdkla f fjdklsa ◊figure-ref{looping-constructs-sample}.\n")
                  (paragraph-indentation t 60 60)
                  (send t get-text))
                (string-append
                 "#lang pollen/markup\n"
                 "\n"
                 "jflkda fkfjdkla f fjdklsa ◊figure-ref{\n"
                 " looping-constructs-sample}.\n"))

  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang pollen/markup\n\ntest1\n     test2\n\t\ttest3\n")
                  (paragraph-indentation t 22 6)
                  (send t get-text))
                "#lang pollen/markup\n\ntest1\ntest2\ntest3\n")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang pollen/markup\n\ntest1\n     test2\n\t\ttest3\n")
                  (paragraph-indentation t 22 20)
                  (send t get-text))
                "#lang pollen/markup\n\ntest1 test2 test3\n")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang pollen/markup\n\ntest1\n     test2\n\t\ttest3 test4\n")
                  (paragraph-indentation t 22 20)
                  (send t get-text))
                "#lang pollen/markup\n\ntest1 test2 test3\ntest4\n")
  
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang pollen/markup\n\ntestcase ◊a{b\n\n\n\n\n      c}\n\n")
                  (paragraph-indentation t 39 14)
                  (send t get-text))
                "#lang pollen/markup\n\ntestcase ◊a{b\n\n\n\n\n c}\n\n")

  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang pollen/markup\n")
                  (send t insert "\n")
                  (send t insert "aa\n")
                  (send t insert "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\n")
                  (send t insert "Hello world lorem ipsum hello world lorem ipsum hi world\n")
                  (send t insert "lorem ipsum hello world lorem ipsum hi world lorem ipsum\n")
                  (send t insert "hello world lorem ipsum hello world lorem ipsum hello\n")
                  (paragraph-indentation t 78 60)
                  (send t get-text))
                (string-append
                 "#lang pollen/markup\n"
                 "\n"
                 "aa\n"
                 "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\n"
                 "Hello world lorem ipsum hello world lorem ipsum hi world\n"
                 "lorem ipsum hello world lorem ipsum hi world lorem ipsum\n"
                 "hello world lorem ipsum hello world lorem ipsum hello\n"))

  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang pollen/markup\n")
                  (send t insert "\n")
                  (send t insert "  aa bb cc\n")
                  (paragraph-indentation t 23 60)
                  (send t get-text))
                (string-append
                 "#lang pollen/markup\n"
                 "\n"
                 "aa bb cc\n"))

  (let ([t (new racket:text%)])
    (insert-them
     t
     "#lang pollen/markdown\n"
     "\n"
     (string-append
      "Abcd abcd abcd abcd abcd ◊racket[hello] abcd abcd abcd abcd. Abcd abcd abcd abcd abcd"
      " abcd abcd abcd abcd abcd abcd abcd.\n"))
    (paragraph-indentation t 57 60)
    (check-equal? (send t get-text)
                  (string-append
                   "#lang pollen/markdown\n"
                   "\n"
                   "Abcd abcd abcd abcd abcd ◊racket[hello] abcd abcd abcd abcd.\n"
                   "Abcd abcd abcd abcd abcd abcd abcd abcd abcd abcd abcd abcd.\n")))

  (let ([t (new racket:text%)])
    (insert-them
     t
     "#lang pollen/markdown\n"
     "\n"
     "◊emph{\n"
     " ◊emph{\n"
     "  ◊emph{\n"
     "   ◊emph{\n"
     "    ◊emph{\n"
     "     blah blah blah blah blah blah blah blah blah blah blah}}}}}")
    (paragraph-indentation t (send t last-position) 60)
    (check-equal? (send t get-text)
                  (string-append
                   "#lang pollen/markdown\n"
                   "\n"
                   "◊emph{ ◊emph{ ◊emph{ ◊emph{ ◊emph{ blah blah blah blah blah\n"
                   "     blah blah blah blah blah blah}}}}}")))

  (let ([t (new racket:text%)])
    (insert-them
     t
     "#lang pollen/markdown\n"
     "\n"
     "◊emph{\n"
     " ◊emph{\n"
     "  ◊emph{\n"
     "   ◊emph{\n"
     "    ◊emph{\n"
     "     blah blah blah blah blah blah blah blah blah blah blah}}}}}")
    (paragraph-indentation t (- (send t last-position) 2) 60)
    (check-equal? (send t get-text)
                  (string-append
                   "#lang pollen/markdown\n"
                   "\n"
                   "◊emph{ ◊emph{ ◊emph{ ◊emph{ ◊emph{ blah blah blah blah blah\n"
                   "     blah blah blah blah blah blah}}}}}")))

  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang pollen/markup\n◊a{b\n  }  \n")
                  (determine-spaces t 26))
                0)
  (check-equal? (let ([t (new racket:text%)])
                  (send t insert "#lang pollen/markup\n◊a{b\n◊{\n}}\n")
                  (determine-spaces t 30))
                0)
  
  ;;test insert-break
  (check-equal? (let ((t (new racket:text%)))
                  (send t insert "aaa bbb ccc ddd")
                  (let ([new-break (insert-break-text t 0 6 14)])
                    (send t delete (add1 new-break) 'back)
                    (send t insert #\newline new-break)
                    (send t get-text))) "aaa\nbbb ccc ddd");;prefer shorter than the "width limit"
  
  ;;for the situation there isn't any #\space on right side 
  (check-equal? (let ((t (new racket:text%)))
                  (send t insert "aaaa bbbb")
                  (let ([new-break (insert-break-text t 0 5 8)])
                    (send t delete (add1 new-break) 'back)
                    (send t insert #\newline new-break)
                    (send t get-text))) "aaaa\nbbbb")
  
  (let ([t (new racket:text%)])
    (define before-newline
      (string-append
       "#lang pollen/markup\n\n"
       "◊hyperlink"
       "[\"http://aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.com\"]{"))
    (define after-newline "link}\n")
    (send t insert (string-append before-newline after-newline))
    (send t freeze-colorer)
    (send t set-position (string-length before-newline) (string-length before-newline))
    (reindent-paragraph t 'whatever-not-an-evt)
    (check-equal? (send t get-text)
                  (string-append before-newline "\n " after-newline))))
