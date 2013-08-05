#lang racket/base
(require (planet mb/pollen/tools) (planet mb/pollen/world))
(require xml xml/path racket/list racket/string)
(require web-server/templates)

; get the values out of the file, or make them up
(define map-file (build-path START_DIR DEFAULT_MAP))
(define map-main empty)

(if (file-exists? map-file)
    ; load it, or ...
    (set! map-main (dynamic-require map-file POLLEN_ROOT)) 
    ; ... synthesize it
    (let ([files (directory-list START_DIR)])
      (set! files (map remove-ext (filter (ƒ(x) (has-ext? x POLLEN_SOURCE_EXT)) files)))
      (set! map-main `(map-main ,@(map path->string files)))))



(define (add-parents x [parent null] [previous null])
  ; disallow main as parent tag
  (when (equal? parent 'map-main) (set! parent empty)) 
  (cond
    [(list? x) 
     (let ([new-parent (car x)])
       ; xexpr with topic as name, parent as attr, children as elements
       `(,@(add-parents new-parent parent) ,@(map (ƒ(i) (add-parents i new-parent)) (cdr x))))]
    [else `(,(as-symbol x) ((parent ,(as-string parent))))]))

(define (remove-parents x)  
  (cond
    [(list? x) `(,(car x) ,@(map remove-parents (cddr x)))]
    [else x]))

(define (main->tree main)
  (add-parents main))

(define tree (main->tree map-main))

(define (get-parent x [xexpr tree])
  (empty/else x (ƒ(x) 
                  (let ([result (se-path* `(,(as-symbol x) #:parent) xexpr)])
                    (if (not result) ; se-path* returns #f if nothing found
                        empty ; but don't pass #f up through the chain.
                        (as-string result))))))

; algorithm to find children
(define (get-children x [xexpr tree])
  (empty/else x (ƒ(x) 
                  ; find contents of node. 
                  (let ([node-contents (se-path*/list `(,(as-symbol x)) xexpr)])
                    ; If there are sublists, just take first element
                    (map (ƒ(i) (as-string (if (list? i) (car i) i))) node-contents)))))

; find all siblings on current level: go up to parent and ask for children
(define (get-all-siblings x [xexpr tree])
  (get-children (get-parent x xexpr) xexpr))

(define (get-adjacent-siblings x [xexpr tree])
  (define-values (left right)
    (splitf-at (get-all-siblings x xexpr) (ƒ(y) (not (equal? (as-string x) (as-string y))))))
  ; use cdr because right piece includes x itself at front
  (values left (empty/else right cdr)))

(define (get-left-siblings x [xexpr tree])
  (define-values (left right) (get-adjacent-siblings x xexpr))
  left)

(define (get-right-siblings x [xexpr tree])
  (define-values (left right) (get-adjacent-siblings x xexpr))
  right)

(define (get-left x [xexpr tree])
  (empty/else (get-left-siblings x xexpr) last))

(define (get-right x [xexpr tree])
  (empty/else (get-right-siblings x xexpr) first))


(define (make-page-sequence [xexpr tree])
  ; use cdr to get rid of body tag at front
  ; todo: calculate exclusions?
  (map as-string (cdr (flatten (remove-parents xexpr))))) 

(define (get-adjacent-pages x [xexpr tree])
  (define-values (left right)
    (splitf-at (make-page-sequence xexpr) (ƒ(y) (not (=str (as-string x) (as-string y))))))
  ; use cdr because right piece includes x itself at front
  (values left (empty/else right cdr)))

(define (get-previous-pages x [xexpr tree])
  (define-values (left right) (get-adjacent-pages x xexpr))
  left)

(define (get-next-pages x [xexpr tree])
  (define-values (left right) (get-adjacent-pages x xexpr))
  right)

(define (get-previous x [xexpr tree])
  (empty/else (get-previous-pages x xexpr) last))

(define (get-next x [xexpr tree])
  (empty/else (get-next-pages x xexpr) first))





(provide (all-defined-out) (all-from-out web-server/templates))