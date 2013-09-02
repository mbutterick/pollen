#lang racket/base
(require racket/list racket/string racket/contract racket/match racket/set)
(require "tools.rkt" "world.rkt" "decode.rkt")

(module+ test (require rackunit))

(provide (all-defined-out))

;; These functions need to be separated so that they can be accessed by pollen parser (in main.rkt)
;; Other pmap functions are in pmap.rkt.
;; pmap decoder takes pmap source and returns a full pmap structure.

;; recursively processes map, converting map locations & their parents into xexprs of this shape:
;; '(location ((parent "parent")))
(define/contract (add-parents x [parent empty])
  ((tagged-xexpr?) (xexpr-tag?) . ->* . pmap?)
  (match x
    ;; this pattern signifies next level in hierarchy 
    ;; where first element is new parent, and rest are children.
    [(list (? xexpr-tag? next-parent) children ...)
     (let-values ([(tag attr _) (break-tagged-xexpr (add-parents next-parent parent))])
       ;; xexpr with tag as name, parent as attr, children as elements with tag as next parent
       (make-tagged-xexpr tag attr (map (λ(c) (add-parents c tag)) children)))]
    ;; single map entry: convert to xexpr with parent
    [else (make-tagged-xexpr (->symbol x) (make-xexpr-attr POLLEN_MAP_PARENT_KEY (->string parent)))]))


;; this sets default input for following functions
(define/contract (pmap-root->pmap tx)
  ;; (not/c pmap) prevents pmaps from being accepted as input
  ((and/c tagged-xexpr? (not/c pmap?)) . -> . pmap?)
  (add-parents tx))


(module+ test
  (define test-pmap-main `(pmap-main "foo" "bar" (one (two "three"))))
  (check-equal? (pmap-root->pmap test-pmap-main) 
                `(pmap-main ((,POLLEN_MAP_PARENT_KEY "")) (foo ((,POLLEN_MAP_PARENT_KEY "pmap-main"))) (bar ((,POLLEN_MAP_PARENT_KEY "pmap-main"))) (one ((,POLLEN_MAP_PARENT_KEY "pmap-main")) (two ((,POLLEN_MAP_PARENT_KEY "one")) (three ((,POLLEN_MAP_PARENT_KEY "two"))))))))



;; contract for pmap-source-decode
(define/contract (valid-pmap-keys? x)
  (any/c . -> . boolean?)
  (andmap (λ(x) (pmap-key? #:loud #t x)) (filter-not whitespace? (flatten x))))

;; contract for pmap-source-decode
(define/contract (unique-pmap-keys? x)
  (any/c . -> . boolean?)
  ;; use map ->string to make keys comparable
  (elements-unique? #:loud #t (map ->string (filter-not whitespace? (flatten x)))))


(define/contract (pmap-source-decode . elements)
  (() #:rest (and/c valid-pmap-keys? unique-pmap-keys?) . ->* . pmap?)
  (pmap-root->pmap (decode (cons POLLEN_MAP_ROOT_NAME elements)
                           #:xexpr-elements-proc (λ(xs) (filter-not whitespace? xs)))))


