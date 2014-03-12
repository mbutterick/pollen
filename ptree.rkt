#lang racket/base
(require racket/path racket/bool)
(require "tools.rkt" "world.rkt" "decode.rkt" sugar txexpr "cache.rkt")


(define+provide (pnode? x)
  (->boolean (try (not (whitespace? (->string x)))
                   (except [exn:fail? (λ(e) #f)]))))


(define+provide/contract (pnode?/error x)
  (any/c . -> . boolean?)
  (or (pnode? x) (error "Not a valid pnode:" x)))


(define+provide (ptree? x)
  (->boolean (and (txexpr? x) (andmap (λ(i) (or (pnode? i) (ptree? i))) x))))


(define+provide/contract (file->ptree p)
  (pathish? . -> . ptree?)
  (cached-require (->path p) world:main-pollen-export))


(define+provide/contract (directory->ptree dir)
  (directory-pathish? . -> . ptree?)
  (let ([files (map remove-ext (filter (λ(x) (has-ext? x world:markup-source-ext)) (directory-list dir)))])
    (ptree-root->ptree (cons world:ptree-root-node files))))


;; Try loading from ptree file, or failing that, synthesize ptree.
(define+provide/contract (make-project-ptree project-dir)
  (directory-pathish? . -> . ptree?)
  (define ptree-source (build-path project-dir world:default-ptree))
  (cached-require ptree-source world:main-pollen-export))


(define+provide/contract (parent pnode [ptree (current-ptree)])
  (((or/c false? pnode?)) (ptree?) . ->* . (or/c false? pnode?)) 
  (and pnode
       (if (member (->string pnode) (map (λ(x) (->string (if (list? x) (car x) x))) (cdr ptree)))
           (->string (car ptree))
           (ormap (λ(x) (parent pnode x)) (filter list? ptree)))))


(define+provide/contract (children pnode [ptree (current-ptree)])
  (((or/c false? pnode?)) (ptree?) . ->* . (or/c false? (listof pnode?)))  
  (and pnode 
       (if (equal? (->string pnode) (->string (car ptree)))
           (map (λ(x) (->string (if (list? x) (car x) x))) (cdr ptree))
           (ormap (λ(x) (children pnode x)) (filter list? ptree)))))


(define+provide/contract (siblings pnode [ptree (current-ptree)])
  (((or/c false? pnode?)) (ptree?) . ->* . (or/c false? (listof string?)))  
  (children (parent pnode ptree) ptree))


;; flatten tree to sequence
(define+provide/contract (ptree->list [ptree (current-ptree)])
  (ptree? . -> . (listof string?))
  ; use cdr to get rid of root tag at front
  (map ->string (cdr (flatten ptree)))) 


(define+provide/contract (adjacents side pnode [ptree (current-ptree)])
  ((symbol? (or/c false? pnode?)) (ptree?) . ->* . (or/c false? (listof pnode?)))
  (and pnode
       (let* ([proc (if (equal? side 'left) takef takef-right)]
              [result (proc (ptree->list ptree) (λ(x) (not (equal? (->string pnode) (->string x)))))])
         (and (not (empty? result)) result))))


(define+provide/contract (left-adjacents pnode [ptree (current-ptree)]) 
  (((or/c false? pnode?)) (ptree?) . ->* . (or/c false? (listof pnode?)))
  (adjacents 'left pnode ptree))


(define+provide/contract (right-adjacents pnode [ptree (current-ptree)]) 
  (((or/c false? pnode?)) (ptree?) . ->* . (or/c false? (listof pnode?)))
  (adjacents 'right pnode ptree))


(define+provide/contract (previous pnode [ptree (current-ptree)])
  (((or/c false? pnode?)) (ptree?) . ->* . (or/c false? pnode?))
  (let ([result (left-adjacents pnode ptree)])
    (and result (last result))))


(define+provide/contract (next pnode [ptree (current-ptree)])
  (((or/c false? pnode?)) (ptree?) . ->* . (or/c false? pnode?))
  (let ([result (right-adjacents pnode ptree)])
    (and result (first result))))


;; this is a helper function to permit unit tests
(define+provide (pnode->url/paths pnode url-list)
  ;; check for duplicates because some sources might have already been rendered
  (define output-paths (remove-duplicates (map ->output-path url-list) equal?))
  (define matching-paths (filter (λ(x) (equal? (->string x) (->string pnode))) output-paths))
  (cond
    [((len matching-paths) . = . 1) (->string (car matching-paths))]
    [((len matching-paths) . > . 1) (error "More than one matching URL for" pnode)]
    [else #f]))


(define+provide/contract (pnode->url pnode [url-context (current-url-context)])
  ((pnode?) (pathish?) . ->* . (or/c false? pnode?))
  (parameterize ([current-url-context url-context])
    (pnode->url/paths pnode (directory-list (current-url-context)))))


;; this sets default input for following functions
(define+provide/contract (ptree-root->ptree tx)
  ;; (not/c ptree) prevents ptrees from being accepted as input
  ((and/c txexpr?) . -> . ptree?)
  tx)


(define+provide/contract (pnodes-unique?/error x)
  (any/c . -> . boolean?)
  (define members (filter-not whitespace? (flatten x)))
  (and (andmap pnode?/error members)
       (members-unique?/error (map ->string members))))


(define+provide/contract (ptree-source-decode . elements)
  (() #:rest pnodes-unique?/error . ->* . ptree?)
  (ptree-root->ptree (decode (cons world:ptree-root-node elements)
                             #:xexpr-elements-proc (λ(xs) (filter-not whitespace? xs)))))


(define current-ptree (make-parameter `(,world:ptree-root-node)))
(define current-url-context (make-parameter (world:current-project-root)))
(provide current-ptree current-url-context)


;; used to convert here-path into here
(define+provide/contract (path->pnode path)
  (pathish? . -> . pnode?)
  (->string (->output-path (find-relative-path (world:current-project-root) (->path path)))))