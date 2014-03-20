#lang racket/base
(require racket/path racket/list)
(require "file.rkt" "world.rkt" "decode.rkt" sugar txexpr "cache.rkt")


(define+provide current-pagetree (make-parameter #f))


(define+provide (pagenode? x)
  (->boolean (and (symbol? x) (try (not (whitespace/nbsp? (->string x)))
                                   (except [exn:fail? (λ(e) #f)])))))


(define+provide (pagenodeish? x)
  (try (pagenode? (->symbol x))
       (except [exn:fail? (λ(e) #f)])))


(define/contract+provide (->pagenode x)
  (pagenodeish? . -> . pagenode?)
  (->symbol x))


(define+provide/contract (decode-pagetree xs)
  (txexpr-elements? . -> . any/c) ; because pagetree is being explicitly validated
  (validate-pagetree 
   (decode (cons world:pagetree-root-node xs)
           #:txexpr-elements-proc (λ(xs) (filter (compose1 not whitespace?) xs))
           #:string-proc string->symbol))) ; because faster than ->pagenode


(define+provide (validate-pagetree x)
  (and (txexpr? x)
       (let ([pagenodes (pagetree->list x)])
         (and
          (andmap (λ(p) (or (pagenode? p) (error (format "validate-pagetree: \"~a\" is not a valid pagenode" p)))) pagenodes)
          (try (members-unique?/error pagenodes)
               (except [exn:fail? (λ(e) (error (format "validate-pagetree: ~a" (exn-message e))))]))
          x))))


(define+provide (pagetree? x)
  (try (->boolean (validate-pagetree x))
       (except [exn:fail? (λ(e) #f)])))


(define+provide/contract (directory->pagetree dir)
  (coerce/path? . -> . pagetree?)
  
  (define (unique-sorted-output-paths xs)
    (define output-paths (map ->output-path xs))
    (define all-paths (filter visible? (remove-duplicates output-paths)))
    (define path-is-directory? (λ(f) (directory-exists? (build-path dir f))))
    (define-values (subdirectories files) (partition path-is-directory? all-paths))
    (define-values (pagetree-sources other-files) (partition pagetree-source? files))
    (define (sort-names xs) (sort xs #:key ->string string<?))
    ;; put subdirs in list ahead of files (so they appear at the top)
    (append (sort-names subdirectories) (sort-names pagetree-sources) (sort-names other-files)))
  
  (if (directory-exists? dir )
      (decode-pagetree (map ->symbol (unique-sorted-output-paths (directory-list dir))))
      (error (format "directory->pagetree: directory ~a doesn't exist" dir)))) 

;; Try loading from pagetree file, or failing that, synthesize pagetree.
(define+provide/contract (make-project-pagetree project-dir)
  (pathish? . -> . pagetree?)
  (with-handlers ([exn:fail? (λ(exn) (directory->pagetree project-dir))])
    (define pagetree-source (build-path project-dir world:default-pagetree))
    (cached-require pagetree-source world:main-pollen-export)))

(define+provide/contract (parent pnish [pt (current-pagetree)])
  (((or/c #f pagenodeish?)) (pagetree?) . ->* . (or/c #f pagenode?)) 
  (and pt pnish
       (let ([pagenode (->pagenode pnish)])
         (if (member pagenode (map (λ(x) (if (list? x) (car x) x)) (cdr pt)))
             (car pt)
             (ormap (λ(x) (parent pagenode x)) (filter list? pt))))))


(define+provide/contract (children p [pt (current-pagetree)])
  (((or/c #f pagenodeish?)) (pagetree?) . ->* . (or/c #f (listof pagenode?)))  
  (and pt p 
       (let ([pagenode (->pagenode p)])
         (if (equal? pagenode (car pt))
             (map (λ(x) (if (list? x) (car x) x)) (cdr pt))
             (ormap (λ(x) (children pagenode x)) (filter list? pt))))))


(define+provide/contract (siblings pnish [pt (current-pagetree)])
  (((or/c #f pagenodeish?)) (pagetree?) . ->* . (or/c #f (listof pagenode?)))  
  (children (parent pnish pt) pt))


;; flatten tree to sequence
(define+provide/contract (pagetree->list pt)
  (pagetree? . -> . (listof pagenode?))
  ; use cdr to get rid of root tag at front
  (cdr (flatten pt))) 


(define (adjacents side pnish [pt (current-pagetree)])
  ; ((symbol? (or/c #f pagenodeish?)) (pagetree?) . ->* . (or/c #f (listof pagenode?)))
  (and pt pnish
       (let* ([pagenode (->pagenode pnish)]
              [proc (if (equal? side 'left) takef takef-right)]
              [result (proc (pagetree->list pt) (λ(x) (not (equal? pagenode x))))])
         (and (not (empty? result)) result))))


(define+provide/contract (previous* pnish [pt (current-pagetree)]) 
  (((or/c #f pagenodeish?)) (pagetree?) . ->* . (or/c #f (listof pagenode?)))
  (adjacents 'left pnish pt))


(define+provide/contract (next* pnish [pt (current-pagetree)]) 
  (((or/c #f pagenodeish?)) (pagetree?) . ->* . (or/c #f (listof pagenode?)))
  (adjacents 'right pnish pt))


(define+provide/contract (previous pnish [pt (current-pagetree)])
  (((or/c #f pagenodeish?)) (pagetree?) . ->* . (or/c #f pagenode?))
  (let ([result (previous* pnish pt)])
    (and result (last result))))


(define+provide/contract (next pnish [pt (current-pagetree)])
  (((or/c #f pagenodeish?)) (pagetree?) . ->* . (or/c #f pagenode?))
  (let ([result (next* pnish pt)])
    (and result (first result))))


(define/contract+provide (path->pagenode path)
  (coerce/path? . -> . coerce/symbol?)
  (->output-path (find-relative-path (world:current-project-root) (->complete-path path))))


(define+provide/contract (in-pagetree? pnish [pt (current-pagetree)])
  (((or/c #f pagenodeish?)) (pagetree?) . ->* . boolean?)
  (->boolean (and pnish (member pnish (pagetree->list pt)))))
