#lang racket/base
(require racket/path)
(require "tools.rkt" "world.rkt" "decode.rkt" sugar txexpr "cache.rkt")


(define+provide current-pagemap (make-parameter #f))


(define+provide (node? x)
  (->boolean (and (symbol? x) (try (not (whitespace/nbsp? (->string x)))
                                   (except [exn:fail? (λ(e) #f)])))))


(define+provide (nodeish? x)
  (try (node? (->symbol x))
       (except [exn:fail? (λ(e) #f)])))


(define/contract+provide (->node x)
  (nodeish? . -> . node?)
  (->symbol x))


(define+provide/contract (decode-pagemap xs)
  (txexpr-elements? . -> . any/c) ; because pagemap is being explicitly validated
  (validate-pagemap 
   (decode (cons world:pagemap-root-node xs)
           #:txexpr-elements-proc (λ(xs) (filter (compose1 not whitespace?) xs))
           #:string-proc string->symbol))) ; because faster than ->node


(define+provide (validate-pagemap x)
  (let ([nodes (pagemap->list x)])
    (and
     (andmap (λ(p) (or (node? p) (error (format "validate-pagemap: \"~a\" is not a valid node" p)))) nodes)
     (try (members-unique?/error nodes)
          (except [exn:fail? (λ(e) (error (format "validate-pagemap: ~a" (exn-message e))))]))
     x)))


(define+provide (pagemap? x)
  (try (->boolean (validate-pagemap x))
       (except [exn:fail? (λ(e) #f)])))


;; Try loading from pagemap file, or failing that, synthesize pagemap.
(define+provide/contract (make-project-pagemap project-dir)
  (pathish? . -> . pagemap?)
  (define pagemap-source (build-path project-dir world:default-pagemap))
  (cached-require pagemap-source world:main-pollen-export))


(define+provide/contract (parent-node p [pagemap (current-pagemap)])
  (((or/c #f nodeish?)) (pagemap?) . ->* . (or/c #f node?)) 
  (and pagemap p
       (let ([node (->node p)])
         (if (member node (map (λ(x) (if (list? x) (car x) x)) (cdr pagemap)))
             (car pagemap)
             (ormap (λ(x) (parent-node node x)) (filter list? pagemap))))))


(define+provide/contract (child-nodes p [pagemap (current-pagemap)])
  (((or/c #f nodeish?)) (pagemap?) . ->* . (or/c #f (listof node?)))  
  (and pagemap p 
       (let ([node (->node p)])
         (if (equal? node (car pagemap))
             (map (λ(x) (if (list? x) (car x) x)) (cdr pagemap))
             (ormap (λ(x) (child-nodes node x)) (filter list? pagemap))))))


(define+provide/contract (sibling-nodes p [pagemap (current-pagemap)])
  (((or/c #f nodeish?)) (pagemap?) . ->* . (or/c #f (listof node?)))  
  (child-nodes (parent-node p pagemap) pagemap))


;; flatten tree to sequence
(define+provide/contract (pagemap->list pagemap)
  (pagemap? . -> . (listof node?))
  ; use cdr to get rid of root tag at front
  (cdr (flatten pagemap))) 


(define (adjacent-nodes side p [pagemap (current-pagemap)])
  ; ((symbol? (or/c #f nodeish?)) (pagemap?) . ->* . (or/c #f (listof node?)))
  (and pagemap p
       (let* ([node (->node p)]
              [proc (if (equal? side 'left) takef takef-right)]
              [result (proc (pagemap->list pagemap) (λ(x) (not (equal? node x))))])
         (and (not (empty? result)) result))))


(define+provide/contract (previous-nodes node [pagemap (current-pagemap)]) 
  (((or/c #f nodeish?)) (pagemap?) . ->* . (or/c #f (listof node?)))
  (adjacent-nodes 'left node pagemap))


(define+provide/contract (next-nodes node [pagemap (current-pagemap)]) 
  (((or/c #f nodeish?)) (pagemap?) . ->* . (or/c #f (listof node?)))
  (adjacent-nodes 'right node pagemap))


(define+provide/contract (previous-node node [pagemap (current-pagemap)])
  (((or/c #f nodeish?)) (pagemap?) . ->* . (or/c #f node?))
  (let ([result (previous-nodes node pagemap)])
    (and result (last result))))


(define+provide/contract (next-node node [pagemap (current-pagemap)])
  (((or/c #f nodeish?)) (pagemap?) . ->* . (or/c #f node?))
  (let ([result (next-nodes node pagemap)])
    (and result (first result))))


(define/contract+provide (path->node path)
  (coerce/path? . -> . coerce/symbol?)
  (->output-path (find-relative-path (world:current-project-root) (->complete-path path))))


(define+provide/contract (node-in-pagemap? node [pagemap (current-pagemap)])
  (((or/c #f nodeish?)) (pagemap?) . ->* . boolean?)
  (->boolean (and node (member node (pagemap->list pagemap)))))
