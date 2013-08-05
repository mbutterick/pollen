#lang racket/base
(require racket/list)
(require xml)
(require (only-in racket/function thunk))
(require racket/string)
(require racket/file)
(require xml/path)
(require (only-in racket/format ~a ~s ~v))
(require (prefix-in scribble: (only-in scribble/decode whitespace?)))
(require (only-in racket/path filename-extension))
(require (only-in (planet mb/pollen/library/html) inline-tags))
(require (planet mb/pollen/world))
(require (planet mb/pollen/readability))
;(require (planet mb/pollen/hyphenate))


(define nbsp " ") ; use this for readability in code
(define lozenge "◊") ; use this instead of escape syntax

(provide (all-defined-out) 
         describe whitespace? xexpr->string xexpr? filter-not flatten
         (all-from-out (planet mb/pollen/readability)))

(define (hash-ref-or hash key [default #f])
  (if (in? hash key)
      (get hash key)
      default))

(define (make-meta-hash x)
  (define keys (se-path*/list '(meta #:name) x))
  (define values (se-path*/list '(meta #:content) x))
  (define meta-hash (make-hash))
  ;todo: convert this to for/list because map does not guarantee ordering
  ; probably want to keep it in sequence
  (map (ƒ(key value) (change meta-hash (as-symbol key) (as-string value))) keys values)
  meta-hash)


(define (magic-directory? path)
  (and (directory-exists? path) 
       (or (ends-with? (path->string path) "requires")
           (ends-with? (path->string path) "compiled")
           )))

(define (filename-of path)
  (let-values ([(dir filename ignored) (split-path path)])
    filename))

(define (pollen-script? path)
  (let ([path-string (path->string (filename-of path))])
    (or (starts-with? path-string "pollen_") (starts-with? path-string "pollen-"))))

(define (racket-file? path)
  (has-ext? path 'rkt))

(define (pmap-source? path)
  (has-ext? path POLLEN_MAP_EXT))

(define (template-source? path)
  (starts-with? (path->string (filename-of path)) (~a TEMPLATE_FILE_PREFIX)))

(define (preproc-source? path)
  (has-ext? path POLLEN_PREPROC_EXT))

(define (make-preproc-in-path path)
  (add-ext path POLLEN_PREPROC_EXT))

(define (make-preproc-out-path path)
  (remove-ext path))

(define (has-preproc-source? path)
  (file-exists? (make-preproc-in-path path)))


(define (pollen-source? path)
  (has-ext? path POLLEN_SOURCE_EXT))

(define (make-pollen-source-path thing)
  (add-ext (remove-ext (as-path thing)) POLLEN_SOURCE_EXT))

(define (has-pollen-source? path)
  (file-exists? (make-pollen-source-path path)))  




(define (insert-subdir path [subdir-in OUTPUT_SUBDIR])
  (let-values ([(dir filename ignored) (split-path path)])
    (when (equal? dir 'relative)
      (set! dir (string->path ".")))
    (letrec ([subdir-name (string->path (~a subdir-in))]
             [subdir (build-path dir subdir-name)])
      (when (not (directory-exists? subdir))
        (make-directory subdir))
      (build-path subdir filename))))


;;;;;;;;;;;;;;
; Moved from template.rkt
;;;;;;;;;;;;;;

; All from* functions should return a named-xexpr

(define (from x query)
  ; cache x
  (let ([x (put x)])
    ; try finding it in metas, if not, find it in main, if not then return false
    (or (from-metas x query) (from-main x query))))

(define (from-main x query) ; this used to be plain from
  ; check results first
  (let* ([x (put x)]
         [results (se-path*/list (list query) x)])
    ; if results exist, send back xexpr as output
    (if (not (empty? results))
        `(,query ,@results) ; todo: why use query as tag?
        #f)))

(define (from-metas x key)
  (let* ([x (put x)]
         [meta-hash (make-meta-hash x)]
         [key (as-symbol key)])
    (if (in? meta-hash key)
        `(value ,(get meta-hash key)) ;todo: why use value as tag?
        #f)))


(define (put x)
  ; handles either xexpr or pollen file as input
  (cond
    ; pass through xexpr as is
    ; put is optional for xexprs.
    ; it's only here to make the idiom smooth.
    [(named-xexpr? x) x] 
    ; todo: how to externalize pollen main tag into world name?
    [(file-exists? (as-path x)) (dynamic-require x 'main)]
    ; also try adding pollen file extension
    ; this makes put compatible with map references
    [(let ([x (make-pollen-source-path x)])
       (when (file-exists? x)
         (put x)))]
    [else (error "put: need named xexpr or pollen file, but got" x)]))


(define (merge x)
  (cond
    [(named-xexpr? x)
     ; return content of xexpr.
     ; pollen language rules will splice these into the main flow.
     (if (empty? x)
         ""
         (let-values([(name attr content) (xexplode x)])
           content))]
    [(string? x) (list x)]))


#|(define (merge-strings x)
  (when (empty? x) (error "merge-strings got empty x"))
  ;todo: filter metas?
  ; leaning toward no. Simplest behavior.
  ; function is not intended to be used with whole pollen body anyhow.
  (let ([x (merge x)])
    (string-join (filter string? (flatten x)) " ")))|#

(define (merge-strings x)
  (string-join (filter string? (flatten x)) " "))


(define (make-html x)
  (if (named-xexpr? x)
      (xexpr->string x)
      (let ([x (as-list x)])
        (when (all xexpr? x)
          (string-join (map xexpr->string x) "")))))

; generate *-as-html versions of functions
(define-values (put-as-html merge-as-html merge-strings-as-html)
  (apply values (map (ƒ(proc) (ƒ(x) (make-html (proc x)))) (list put merge merge-strings))))


(define (as-literal x)
  (set! x (flatten (list x))) ; coerce text or list to new list
  (merge `(literal-thing ,@x)))

(define (make-url x)
  (if (exists? x)
      (str x ".html")
      "#")) ; funny null url that means "stay here"


;;;;;;;;;;;;;;;;;;;;;;;;;


; make these independent of local includes
(define (map-topic topic . subtopics)
  `(,(string->symbol topic) ,@(filter-not whitespace? subtopics)))

(define (meta key value) 
  `(meta ((name ,(as-string key))(content ,(as-string value)))))

; scribble's whitespace function misses trailing spaces wrapped in a list
(define (whitespace? x)
  (cond
    [(list? x) (all scribble:whitespace? x)]
    [else (scribble:whitespace? x)]))



; remove empty elements
(define (remove-empty x)
  (cond
    [(list? x) (map remove-empty (filter-not empty? x))]
    [else x]))

(define (remove-void x)
  (cond
    [(list? x) (map remove-void (filter-not void? x))]
    [else x]))

; common idiom with lists:
; if list is empty, return empty
; otherwise do procedure
(define (empty/else thing proc)
  (if (empty? thing)
      empty
      (proc thing)))

; common idiom with files:
; if the file exists, do procedure with it
(define (file-exists?/do path proc)
  (if (file-exists? path)
      (proc path)
      #f))

; simple timer
(define-syntax-rule (time expr)
  (begin 
    (define start-time (current-inexact-milliseconds))
    (define result expr)
    (define stop-time (current-inexact-milliseconds))
    (message "Time for" 'expr "=" (- stop-time start-time)) 
    result))


; utilities for working with file extensions
(define (.+ x) (format ".~a" x))

(define (get-ext path)
  (bytes->string/utf-8 (filename-extension path)))

(define (has-ext? path ext)
  (let ([path-ext (filename-extension path)])
    ; returns true if f-ext exists, and equals ext, otherwise false
    (and path-ext (equal? (bytes->string/utf-8 path-ext) (~a ext)))))

(define (remove-ext path)
  (path-replace-suffix path ""))

(define (add-ext path ext)
  (string->path (string-append (path->string path) (.+ ext))))

; find all xexpr names within another xexpr
(define (gather-xexpr-names x)
  (cond
    [(named-xexpr? x) 
     (let-values([(name attr content) (xexplode x)])
       (flatten (cons name (map gather-xexpr-names content))))] 
    [else empty]))



; shorthand for define + dynamic require
(define-syntax-rule (define-from module symbol)
  (define symbol (dynamic-require module 'symbol)))

; dynamic require or return false if not found
; allows constructs like:
; (or (require-now module symbol) "default value")
(define (require-now module symbol)
  (dynamic-require module symbol (thunk #f)))


; define & provide in one easy step
(define-syntax-rule (define/provide name expr ...)
  (begin (define name expr ...)(provide name)))

; xexpr->html
(define (xexpr->html x)
  ;  (string-join (map xexpr->string x)))
  (xexpr->string x))

(define (html->xexpr . stuff)
  (string->xexpr (string-join stuff "")))

; do a set of actions on same item
(define ((tee . procs) thing)
  (apply values (map (ƒ(proc)(proc thing)) procs)))

; python-style try/except syntax
(define-syntax-rule (try body (except tests ...))
  (with-handlers (tests ...) body))

; xexpr shortcut; map tag across items
(define (map-tag tag-name xs)
  (map (ƒ(x) (list tag-name x)) xs))


; trim from beginning & end of list
(define (trim things test)
  (dropf-right (dropf things test) test))

; trim whitespace from beginning & end of list
(define (trim-whitespace things)
  (trim things whitespace?))


; ----------------------------
; DECODER
; ----------------------------

(define (splice-xexpr-content x [acc '()])
  ; takes a list and splices top-level sublists into main list
  ; used by merge function
  (cond
    [(empty? x) acc]
    [(and (xexpr-content? (car x)) (not (named-xexpr? (car x)))) (splice-xexpr-content (cdr x) `(,@acc ,@(car x)))]
    [else (splice-xexpr-content (cdr x) `(,@acc ,(car x)))]))


(define (named-xexpr? x)
  ; meets basic xexpr contract, and is also a list starting with a symbol
  ; todo: rewrite this using match?
  ; todo: rewrite this recurively so errors can be pinpointed (for debugging)
  (and (xexpr? x) (list? x) (symbol? (car x))))

(define (xexpr-attr-list? x)
  (define (attr-pair? x)
    ; list with two elements: first element is a symbol, second is a string
    (and (list? x) (= (length x) 2) (symbol? (car x)) (string? (second x))))   
  ; a list where elements are attr pairs
  (and (list? x) (all attr-pair? x)))

(define (xexpr-content? x) 
  ; it's a list whose elements meet xexpr contract
  (and (list? x) (all xexpr? x)))

(define (xexpr-has-attrs? x)
  (and (named-xexpr? x) (> (length x) 1) (xexpr-attr-list? (second x)))) 

(define (make-xexpr name (attr empty) (content empty)) 
  (when (not (symbol? name)) (error "make-xexpr: need a name, dude"))
  (when (not (xexpr-attr-list? attr)) 
    (error "make-xexpr: attr must be list of attr pairs"))
  ; todo: fix xexpr-content? test so I can use it here
  ; (when (not (xexpr-content? content)) content)
  (when (not (list? content)) (error "make-xexpr: content must be a list"))
  
  (define xexpr `(,name))
  (when (exists? attr) (set! xexpr `(,@xexpr ,attr)))
  (when (exists? content) (set! xexpr `(,@xexpr ,@content)))
  xexpr)

(define (xexplode x)
  (when (not (named-xexpr? x)) (error (format "xexplode: ~v not a named-xexpr" x)))
  (define-values (name attr content) (values (car x) empty empty))  
  (if (xexpr-has-attrs? x)
      (set!-values (attr content) (values (second x) (cddr x))) ; attr comes back as a list of lists
      (set! content (cdr x))) ; content always comes back as a list
  (values name attr content)) 

; block is a named expression that's not on the inline list
; todo: bear in mind that browsers take the opposite view:
; that only elements on the block list are blocks
; and otherwise are treated as inline 
(define (block-xexpr? x)
  (and (named-xexpr? x) (not (in? inline-tags (car x)))))

(define (wrap-paragraph x) ; x is a list containing paragraph pieces
  ; if paragraph is just one block-level xexpr
  (if (and (= (length x) 1) (block-xexpr? (car x))) 
      (car x) ; leave it
      `(p ,@x))) ; otherwise wrap in p tag



; wrap initial quotes for hanging punctuation
; todo: improve this
; does not handle <p>“<em>thing</em> properly
(define (wrap-hanging-quotes x) ; x is one paragraph
  (define-values (name attr content) (xexplode x))
  (cond 
    [(and (not (empty? content)) 
          (string? (car content)) 
          (> (string-length (car content)) 1))
     (let ([new-car 
            (letrec ([x (car content)] 
                     [first (get x 0)] 
                     [rest (get x 1 'end)])
              (cond
                [(member first '("\"" "“"))
                 ; this has to be span so that it's explicitly 
                 ; an inline element. If not,
                 ; things like linebreak detection won't work.
                 `(span ((class "dquo")) ,(~a #\“) ,rest)]
                [(member first '("\'" "‘")) 
                 `(span ((class "squo")) ,(~a #\‘) ,rest)]
                [else x]))])
       (make-xexpr name attr (cons new-car (cdr content))))]
    [(and (exists? content) (named-xexpr? (car content)))
     (make-xexpr name attr (cons (wrap-hanging-quotes (car content)) (cdr content)))]
    [else x]))



; how a list-item break is denoted: three or more newlines
(define (list-item-break? x)
  (and (string? x) (regexp-match #rx"^\n\n\n+$" x)))

; how a paragraph break is denoted: two or more newlines
(define (paragraph-break? x)
  ;  (equal? x PARAGRAPH_BREAK) ; obsolete: two newlines only
  (and (string? x) (regexp-match #rx"^\n\n+$" x)))

; convert single newline to br tag
; only if neither adjacent tag is a block
; otherwise delete
(define (convert-linebreaks x)  ; x is list
  (remove-empty
   (for/list ([i (len x)])
     (cond
       [(equal? (get x i) LINE_BREAK)
        (if (none block-xexpr? (list (get x (sub1 i)) (get x (add1 i))))
            '(br)
            '())]
       [else (get x i)]))))

; find two or more adjacent newlines and bring them together 
; works on any number of newlines
(define (merge-newlines x)
  (define (newline? x)
    (and (string? x) (equal? "\n" x)))
  (define (not-newline? x)
    (not (newline? x)))
  
  (define (merge-newlines-inner x [acc '()]) ; x is list
    (if (empty? x)
        acc
        (let-values ([(leading-newlines remainder) (splitf-at x newline?)])
          (if (not (empty? leading-newlines))
              (merge-newlines-inner remainder `(,@acc  ,(string-join leading-newlines "")))
              (merge-newlines-inner (dropf remainder not-newline?) `(,@acc ,@(takef remainder not-newline?)))))))
  
  (cond
    ((list? x) (merge-newlines-inner (map merge-newlines x)))
    (else x)))

(define (typogrify string)
  ; make set of functions for replacers
  (define (make-replacers query+subs)
    (map (ƒ(q+s) (ƒ(str) (regexp-replace* (first q+s) str (second q+s)))) query+subs))
  
  ; just store the query strings + replacement strings
  (define dashes 
    ; fix em dashes first, else they'll be mistaken for en dashes
    ; [\\s ] is whitespace + nonbreaking space
    '((#px"[\\s ]*(---|—)[\\s ]*" "—") ; em dash
      (#px"[\\s ]*(--|–)[\\s ]*" "–"))) ; en dash
  
  (define smart-quotes
    '((#px"(?<=\\w)'(?=\\w)" "’") ; apostrophe
      (#px"(?<!\\w)'(?=\\w)" "‘") ; single_at_beginning
      (#px"(?<=\\S)'(?!\\w)" "’") ; single_at_end
      (#px"(?<!\\w)\"(?=\\w)" "“") ; double_at_beginning
      (#px"(?<=\\S)\"(?!\\w)" "”"))) ; double_at_end
  
  ; todo: is this transformation obsolete due to css ligatures?
  ; maybe not, because soft hyphens mess up css ligature function.
  ; \u00AD is a soft hyphen, which might appear in between letters
  (define ligatures
    '((#px"f\u00AD?i" "ﬁ")
      (#px"f\u00AD?f" "ﬀ")
      (#px"f\u00AD?l" "ﬂ")
      (#px"f\u00AD?f\u00AD?i" "ﬃ")
      (#px"f\u00AD?f\u00AD?l" "ﬄ")))
  
  ; put replacers in desired order here
  (define replacers (make-replacers (append dashes smart-quotes))) 
  ; compose goes from last to first, so reverse order
  ;  ((apply compose1 hyphenate-text-soft (reverse replacers)) string))
  ((apply compose1 (reverse replacers)) string))


; find the last word space and replace it with a nonbreaking space
; doesn't work on weirdo cases that need backtracking, like:
; (define t4 '(p "hello from all the freaks" "at the " (em "factory.")))
; but cures enough problems to be worthwhile.

(define (nonbreaking-last-space x)
  (define nbsp #\ ) ; use an Ø if you want to make the results visible
  (define minimum-word-length (add1 5)) ; add1 to account for final punctuation
  ; todo: parameterize this, as it will be different for each project
  (define tags-to-pay-attention-to '(p aside)) ; only apply to paragraphs
  
  (define (replace-last-space str)
    (if (in? str #\space)
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
       (let-values([(name attr content) (xexplode x)])
         (if (> (length content) 0) ; content is list of xexprs
             (let-values ([(all-but-last last) (split-at content (sub1 (length content)))]) 
               (make-xexpr name attr `(,@all-but-last ,(find-last-word-space (car last)))))
             x))]
      [else x]))
  
  (if (in? tags-to-pay-attention-to (car x))
      (find-last-word-space x)
      x))

(define (prep-paragraph-flow x)
  (convert-linebreaks (merge-newlines (trim-whitespace x))))

(define (map-xexpr-content proc x #:only [only-proc (ƒ(x) x)])
  ; why map and not apply? Because map guarantees a list of the same length.
  ; whereas apply does not. So it works as an implied constraint.
  (if (named-xexpr? x)
      (let-values([(name attr content) (xexplode x)]) 
        (make-xexpr name attr (map (ƒ(x) (if (only-proc x) (proc x) x)) content)))
      (error "map-xexpr-content: Input is not a named xexpr and has no content:" x)))


; Main decode function
(define (decode x)
  (define (&decode x)
    (cond
      [(named-xexpr? x)
       (let-values([(name attr content) (xexplode x)]) 
         (define decoded-x (make-xexpr name attr (&decode content)))
         (if (block-xexpr? decoded-x)
             ; add nonbreaking-last-space to the next line when ready
             (wrap-hanging-quotes (nonbreaking-last-space decoded-x)) ; do special processing for block xexprs
             decoded-x))]
      [(xexpr-content? x) ; a list of xexprs
       (let ([x (prep-paragraph-flow x)]) 
         (map &decode (if (any paragraph-break? x) ; need this condition to prevent infinite recursion
                          (map wrap-paragraph (splitf-at* x paragraph-break?)) ; split into ¶¶
                          x)))]     
      [(string? x) (typogrify x)]
      [else x]))
  
  (define (stringify x) ; convert numbers to strings
    (cond
      [(list? x) (map stringify x)]
      [(number? x) (~a x)]
      [else x]))
  
  (let* ([x (stringify x)]
         [x (trim-whitespace x)])
    (if (named-xexpr? x)
        (&decode x)
        ;todo: improve this error message, more specific location
        ; now, it just spits out the whole defective content
        (error (format "decode: ~v not a full named-xexpr" x)))))


(define (splitf-at* pieces test)
  ; split list into list of sublists using test
  (define (splitf-at*-inner pieces [acc '()]) ; use acc for tail recursion
    (if (empty? pieces) 
        acc
        (let-values ([(item rest) 
                      (splitf-at (dropf pieces test) (compose1 not test))])
          (splitf-at*-inner rest `(,@acc ,item)))))
  (splitf-at*-inner (trim pieces test)))

(define (make-missing-source-files map-xexpr)
  ; use cdr to omit body tag
  (define source-names (map (ƒ(x) (add-ext (string->path (as-string x)) POLLEN_SOURCE_EXT)) (flatten (cdr map-xexpr))))
  (define (make-source-if-missing x)
    (if (not (file-exists? x))
        (begin
          (display-to-file MISSING_FILE_BOILERPLATE x)
          (format "Created file: ~a" x))
        (format "Already exists: ~a" x)))
  (display (string-join (map make-source-if-missing source-names) "\n")))

