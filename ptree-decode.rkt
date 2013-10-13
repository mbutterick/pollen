#lang racket/base
(require racket/list racket/string racket/contract racket/match racket/set)
(require "tools.rkt" "world.rkt" "decode.rkt")

(module+ test (require rackunit))

(provide (all-defined-out))

;; These functions need to be separated so that they can be accessed by pollen parser (in main.rkt)
;; Other ptree functions are in ptree.rkt.
;; ptree decoder takes ptree source and returns a full ptree structure.

;; recursively processes tree, converting tree locations & their parents into xexprs of this shape:
;; '(location ((parent "parent")))
(define/contract (add-parents x [parent empty])
  ((tagged-xexpr?) (xexpr-tag?) . ->* . ptree?)
  (match x
    ;; this pattern signifies next level in hierarchy 
    ;; where first element is new parent, and rest are children.
    [(list (? xexpr-tag? next-parent) children ...)
     (let-values ([(tag attr _) (break-tagged-xexpr (add-parents next-parent parent))])
       ;; xexpr with tag as name, parent as attr, children as elements with tag as next parent
       (make-tagged-xexpr tag attr (map (λ(c) (add-parents c tag)) children)))]
    ;; single map entry: convert to xexpr with parent
    [else (make-tagged-xexpr (->symbol x) (make-xexpr-attr POLLEN_TREE_PARENT_NAME (->string parent)))]))


;; this sets default input for following functions
(define/contract (ptree-root->ptree tx)
  ;; (not/c ptree) prevents ptrees from being accepted as input
  ((and/c tagged-xexpr? (not/c ptree?)) . -> . ptree?)
  (add-parents tx))


(module+ test
  (define test-ptree-main `(ptree-main "foo" "bar" (one (two "three"))))
  (check-equal? (ptree-root->ptree test-ptree-main) 
                `(ptree-main ((,POLLEN_TREE_PARENT_NAME "")) (foo ((,POLLEN_TREE_PARENT_NAME "ptree-main"))) (bar ((,POLLEN_TREE_PARENT_NAME "ptree-main"))) (one ((,POLLEN_TREE_PARENT_NAME "ptree-main")) (two ((,POLLEN_TREE_PARENT_NAME "one")) (three ((,POLLEN_TREE_PARENT_NAME "two"))))))))



;; contract for ptree-source-decode
(define/contract (valid-pnodes? x)
  (any/c . -> . boolean?)
  (andmap (λ(x) (pnode? #:loud #t x)) (filter-not whitespace? (flatten x))))

;; contract for ptree-source-decode
(define/contract (unique-pnodes? x)
  (any/c . -> . boolean?)
  ;; use map ->string to make keys comparable
  (elements-unique? #:loud #t (map ->string (filter-not whitespace? (flatten x)))))


(define/contract (ptree-source-decode . elements)
  (() #:rest (and/c valid-pnodes? unique-pnodes?) . ->* . ptree?)
  (ptree-root->ptree (decode (cons POLLEN_TREE_ROOT_NAME elements)
                           #:xexpr-elements-proc (λ(xs) (filter-not whitespace? xs)))))


