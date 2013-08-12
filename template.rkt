#lang racket/base
(require xml xml/path racket/list racket/string racket/contract)
(require (except-in web-server/templates in))
(require "tools.rkt" "world.rkt")

(module+ test (require rackunit)
  (define tt (main->tree (dynamic-require "tests/test.pmap" POLLEN_ROOT))))

; get the values out of the file, or make them up
(define map-file (build-path START_DIR DEFAULT_MAP))
(define map-main empty)

;; todo: this ain't a function
(if (file-exists? map-file)
    ; load it, or ...
    (set! map-main (dynamic-require map-file POLLEN_ROOT)) 
    ; ... synthesize it
    (let ([files (directory-list START_DIR)])
      (set! files (map remove-ext (filter (λ(x) (has-ext? x POLLEN_SOURCE_EXT)) files)))
      (set! map-main `(map-main ,@(map path->string files)))))

;; todo: restrict this test
(define/contract (pmap-tree? x)
  (any/c . -> . boolean?)
  (tagged-xexpr? x))

;; insert parents into pmap tree as attrs
(define/contract (add-parents x [parent null] [previous null])
  ((pmap-tree?) (xexpr-tag? xexpr-tag?) . ->* . pmap-tree?)
  ; disallow main as parent tag
  (when (equal? parent 'map-main) (set! parent empty)) 
  (cond
    [(list? x) 
     (let ([new-parent (car x)])
       ; xexpr with topic as name, parent as attr, children as elements
       `(,@(add-parents new-parent parent) ,@(map (λ(i) (add-parents i new-parent)) (cdr x))))]
    [else `(,(->symbol x) ((parent ,(->string parent))))]))

(module+ test
  (define stt `(map-main "foo" ,(map-topic "one" "two")))
  (check-equal? (add-parents stt) '(map-main
                                    ((parent ""))
                                    (foo ((parent "")))
                                    (one ((parent "")) (two ((parent "one")))))))
  
  (define (remove-parents x)  
    (cond
      [(list? x) `(,(car x) ,@(map remove-parents (cddr x)))]
      [else x]))
  
  
  (define (main->tree main)
    (add-parents main))
  
  
  
  
  (define tree (main->tree map-main))
  
  (define (get-parent x [xexpr tree])
    (if (empty? x)
        empty
        (let ([result (se-path* `(,(->symbol x) #:parent) xexpr)])
          (if (not result) ; se-path* returns #f if nothing found
              empty ; but don't pass #f up through the chain.
              (->string result)))))
  
  ; algorithm to find children
  (define (get-children x [xexpr tree])
    (if (empty? x)
        empty
        ; find contents of node. 
        (let ([node-contents (se-path*/list `(,(->symbol x)) xexpr)])
          ; If there are sublists, just take first element
          (map (λ(i) (->string (if (list? i) (car i) i))) node-contents))))
  
  ; find all siblings on current level: go up to parent and ask for children
  (define (get-all-siblings x [xexpr tree])
    (get-children (get-parent x xexpr) xexpr))
  
  (define (get-adjacent-siblings x [xexpr tree])
    (define-values (left right)
      (splitf-at (get-all-siblings x xexpr) (λ(y) (not (equal? (->string x) (->string y))))))
    ; use cdr because right piece includes x itself at front
    (values left (if (empty? right)
                     empty
                     (cdr right))))
  
  (define (get-left-siblings x [xexpr tree])
    (define-values (left right) (get-adjacent-siblings x xexpr))
    left)
  
  (define (get-right-siblings x [xexpr tree])
    (define-values (left right) (get-adjacent-siblings x xexpr))
    right)
  
  (define (get-left x [xexpr tree])
    (if (empty? (get-left-siblings x xexpr))
        empty
        (last (get-left-siblings x xexpr))))
  
  (define (get-right x [xexpr tree])
    (if (empty? (get-right-siblings x xexpr))
        empty
        (first (get-right-siblings x xexpr))))
  
  
  (define (make-page-sequence [xexpr tree])
    ; use cdr to get rid of body tag at front
    ; todo: calculate exclusions?
    (map ->string (cdr (flatten (remove-parents xexpr))))) 
  
  (define (get-adjacent-pages x [xexpr tree])
    (define-values (left right)
      (splitf-at (make-page-sequence xexpr) (λ(y) (not (equal? (->string x) (->string y))))))
    ; use cdr because right piece includes x itself at front
    (values left (if (empty? right)
                     empty
                     (cdr right))))
  
  (define (get-previous-pages x [xexpr tree])
    (define-values (left right) (get-adjacent-pages x xexpr))
    left)
  
  (define (get-next-pages x [xexpr tree])
    (define-values (left right) (get-adjacent-pages x xexpr))
    right)
  
  (define (get-previous x [xexpr tree])
    (if (empty? (get-previous-pages x xexpr))
        empty
        (last (get-previous-pages x xexpr))))
  
  (define (get-next x [xexpr tree])
    (if (empty? (get-next-pages x xexpr))
        empty
        (first (get-next-pages x xexpr))))
  
  
  
  
  
  (provide (all-defined-out) (all-from-out web-server/templates))