#lang racket/base
(require racket/path
         racket/list
         racket/match
         sugar/coerce
         sugar/define
         sugar/test
         sugar/list
         txexpr/base)
(require "setup.rkt"
         "private/whitespace.rkt"
         "private/file-utils.rkt"
         "decode.rkt"
         "cache.rkt")

(define+provide current-pagetree (make-parameter #f))

(define+provide (pagenode? x)
  (->boolean (and (symbol? x) (not (whitespace/nbsp? (symbol->string x))))))

(module-test-external
 (check-false (pagenode? "foo-bar"))
 (check-false (pagenode? "Foo_Bar_0123"))
 (check-true (pagenode? 'foo-bar))
 (check-false (pagenode? "foo-bar.p"))
 (check-false (pagenode? "/Users/MB/foo-bar"))
 (check-false (pagenode? #f))
 (check-false (pagenode? ""))
 (check-false (pagenode? " ")))


;; for contracts: faster than (listof pagenode?)
(define (pagenodes? x)
  (and (list? x) (andmap pagenode? x)))


(define+provide (pagenodeish? x)
  (with-handlers ([exn:fail? (λ (e) #f)]) 
    (->boolean (->pagenode x))))


(define+provide (->pagenode x)
  (with-handlers ([exn:fail? (λ (e) (raise-argument-error '->pagenode "can't convert input to pagenode" x))]) 
    (->symbol x)))


(define+provide/contract (decode-pagetree xs)
  (txexpr-elements? . -> . any/c) ; because pagetree is being explicitly validated
  (define pt-root-tag (setup:pagetree-root-node))
  (define (splice-nested-pagetree xs)
    (apply append (for/list ([x (in-list xs)])
                    (if (and (txexpr? x) (eq? (get-tag x) pt-root-tag))
                        (get-elements x)
                        (list x)))))
  (validate-pagetree 
   (decode (cons pt-root-tag xs)
           #:txexpr-elements-proc (compose1 splice-nested-pagetree (λ (xs) (filter-not whitespace? xs))) 
           #:string-proc string->symbol))) ; because faster than ->pagenode


(define+provide (validate-pagetree x)
  (and (txexpr? x)
       (let ([pagenodes (pagetree-strict->list x)])
         (for/and ([p (in-list pagenodes)]
                   #:unless (pagenode? p))
           (error 'validate-pagetree "~v is not a valid pagenode" p))
         (with-handlers ([exn:fail? (λ (e) (error 'validate-pagetree "~a" (exn-message e)))])
           (members-unique?/error pagenodes))
         x)))


(define+provide (pagetree? x)
  (with-handlers ([exn:fail? (λ (e) #f)]) 
    (->boolean (validate-pagetree x))))


(module-test-external
 (check-true (pagetree? '(foo)))
 (check-true (pagetree? '(foo (hee))))
 (check-true (pagetree? '(foo (hee (uncle foo)))))
 (check-false (pagetree? '(foo (hee hee (uncle foo)))))
 (check-equal? (decode-pagetree '(one two (pagetree-root three (pagetree-root four five) six) seven eight))
               '(pagetree-root one two three four five six seven eight)))


(define+provide/contract (directory->pagetree dir)
  (coerce/path? . -> . pagetree?)
  
  (define (unique-sorted-output-paths xs)
    (define output-paths (map ->output-path xs))
    (define all-paths (filter path-visible? (remove-duplicates output-paths)))
    (define path-is-directory? (λ (f) (directory-exists? (build-path dir f))))
    (define-values (subdirectories files) (partition path-is-directory? all-paths))
    (define-values (pagetree-sources other-files) (partition pagetree-source? files))
    (define (sort-names xs) (sort xs #:key ->string string<?))
    ;; put subdirs in list ahead of files (so they appear at the top)
    (append (sort-names subdirectories) (sort-names pagetree-sources) (sort-names other-files)))
  
  ;; in general we don't filter the directory list for the automatic pagetree.
  ;; this can be annoying sometimes but it's consistent with the policy of avoiding magic behavior.
  ;; certain files (leading dot) will be ignored by `directory-list` anyhow.
  ;; we will, however, ignore Pollen's cache files, and Racket's `compiled` dirs,
  ;; because those shouldn't be project-manipulated.
  (define (cache-dir? path) (member (->string path) default-cache-names))
  
  (unless (directory-exists? dir)
    (error 'directory->pagetree "directory ~v doesn't exist" dir))
  
  (decode-pagetree (map ->pagenode (unique-sorted-output-paths (filter-not cache-dir? (directory-list dir)))))) 


(define+provide/contract (get-pagetree source-path)
  ((or/c pagetree? pathish?) . -> . pagetree?)
  ((if (pagetree? source-path) values cached-doc) source-path))


(define+provide load-pagetree get-pagetree) ; bw compat


;; Try loading from pagetree file, or failing that, synthesize pagetree.
(define+provide/contract (make-project-pagetree project-dir)
  (pathish? . -> . pagetree?)
  (with-handlers ([exn:fail? (λ (exn) (directory->pagetree project-dir))])
    (define pagetree-source (build-path project-dir (setup:main-pagetree)))
    (load-pagetree pagetree-source)))


(define (topmost-node x) (car (->list x)))


(define+provide/contract (parent pnish [pt-or-path (current-pagetree)] #:allow-root [allow-root? #f])
  (((or/c #f pagenodeish?)) ((or/c pagetree? pathish?) #:allow-root boolean?) . ->* . (or/c #f pagenode?))
  (define pt (get-pagetree pt-or-path))
  (define result
    (and pnish
         (let loop ([pagenode (->pagenode pnish)][subtree pt])
           (match-define (list* current-parent current-children) subtree)
           (if (memq pagenode (map topmost-node current-children))
               current-parent
               (for/or ([st (in-list (filter list? current-children))])
                 (loop pagenode st))))))
  (if (eq? result (car pt))
      (and allow-root? result)
      result))


(module-test-external
 (define test-pagetree `(pagetree-main foo bar (one (two three))))
 (check-equal? (parent 'three test-pagetree) 'two)
 (check-equal? (parent "three" test-pagetree) 'two)
 (check-false (parent 'foo test-pagetree))
 (check-false (parent #f test-pagetree))
 (check-false (parent 'nonexistent-name test-pagetree)))


(define+provide/contract (children p [pt-or-path (current-pagetree)])
  (((or/c #f pagenodeish?)) ((or/c pagetree? pathish?)) . ->* . (or/c #f pagenodes?))  
  (and pt-or-path p 
       (let loop ([pagenode (->pagenode p)]
                  [pt (get-pagetree pt-or-path)])
         (if (eq? pagenode (car pt))
             (map topmost-node (cdr pt))
             (for/or ([subtree (in-list (filter pair? pt))])
               (loop pagenode subtree))))))


(module-test-external
 (define test-pagetree `(pagetree-main foo bar (one (two three))))
 (check-equal? (children 'one test-pagetree) '(two))
 (check-equal? (children 'two test-pagetree) '(three))
 (check-false (children 'three test-pagetree))
 (check-false (children #f test-pagetree))
 (check-false (children 'fooburger test-pagetree)))


(define+provide/contract (siblings pnish [pt-or-path (current-pagetree)])
  (((or/c #f pagenodeish?)) ((or/c pagetree? pathish?)) . ->* . (or/c #f pagenodes?))  
  (define pt (get-pagetree pt-or-path))
  (children (parent #:allow-root #t pnish pt) pt))


(module-test-external
 (define test-pagetree `(pagetree-main foo bar (one (two three))))
 (check-equal? (siblings 'one test-pagetree) '(foo bar one))
 (check-equal? (siblings 'foo test-pagetree) '(foo bar one))
 (check-equal? (siblings 'two test-pagetree) '(two))
 (check-false (siblings #f test-pagetree))
 (check-false (siblings 'invalid-key test-pagetree)))


(define+provide/contract (other-siblings pnish [pt-or-path (current-pagetree)])
  (((or/c #f pagenodeish?)) ((or/c pagetree? pathish?)) . ->* . (or/c #f pagenodes?))  
  (match (for/list ([sib (in-list (or (siblings pnish pt-or-path) empty))]
                    #:unless (eq? sib (->pagenode pnish)))
           sib)
    [(? pair? sibs) sibs]
    [else #f]))


(module-test-external
 (define test-pagetree `(pagetree-main foo bar (one (two three four))))
 (check-equal? (other-siblings 'one test-pagetree) '(foo bar))
 (check-equal? (other-siblings 'foo test-pagetree) '(bar one))
 (check-equal? (other-siblings 'two test-pagetree) #f)
 (check-equal? (other-siblings 'three test-pagetree) '(four))
 (check-false (other-siblings #f test-pagetree))
 (check-false (other-siblings 'invalid-key test-pagetree)))


;; private helper function.
;; only takes pt as input.
;; used by `pagetree?` predicate, so can't use `pagetree?` contract.
(define (pagetree-strict->list pt) (flatten (cdr pt)))


;; flatten tree to sequence
(define+provide/contract (pagetree->list pt-or-path)
  ((or/c pagetree? pathish?) . -> . pagenodes?)
  ; use cdr to get rid of root tag at front
  (pagetree-strict->list (get-pagetree pt-or-path))) 


(module-test-external
 (define test-pagetree `(pagetree-main foo bar (one (two three))))
 (check-equal? (pagetree->list test-pagetree) '(foo bar one two three)))


(define (adjacents side pnish [pt-or-path (current-pagetree)])
  (and pt-or-path pnish
       (let loop ([side side]
                  [pagenode (->pagenode pnish)]
                  [pagetree-nodes (pagetree->list (get-pagetree pt-or-path))])
         (if (eq? side 'right)
             (match (memq pagenode pagetree-nodes)
               [(list _ rest ...) rest]
               [else #f])
             (match (loop 'right pagenode (reverse pagetree-nodes))
               [(? pair? result) (reverse result)]
               [else #f])))))


(module-test-internal
 (require rackunit)
 (check-equal? (adjacents 'right 'one '(pagetree-index one two three)) '(two three))
 (check-false (adjacents 'right 'node-not-in-pagetree '(pagetree-index one two three))))


(define+provide/contract (previous* pnish [pt-or-path (current-pagetree)]) 
  (((or/c #f pagenodeish?)) ((or/c pagetree? pathish?)) . ->* . (or/c #f pagenodes?))
  (adjacents 'left pnish pt-or-path))


(module-test-external
 (define test-pagetree `(pagetree-main foo bar (one (two three))))
 (check-equal? (previous* 'one test-pagetree) '(foo bar))
 (check-equal? (previous* 'three test-pagetree) '(foo bar one two))
 (check-false (previous* 'foo test-pagetree)))


(define+provide/contract (next* pnish [pt-or-path (current-pagetree)]) 
  (((or/c #f pagenodeish?)) ((or/c pagetree? pathish?)) . ->* . (or/c #f pagenodes?))
  (adjacents 'right pnish pt-or-path))


(define+provide/contract (previous pnish [pt-or-path (current-pagetree)])
  (((or/c #f pagenodeish?)) ((or/c pagetree? pathish?)) . ->* . (or/c #f pagenode?))
  (match (previous* pnish pt-or-path)
    [(list _ ... result) result]
    [else #f]))


(module-test-external
 (define test-pagetree `(pagetree-main foo bar (one (two three))))
 (check-equal? (previous 'one test-pagetree) 'bar)
 (check-equal? (previous 'three test-pagetree) 'two)
 (check-false (previous 'foo test-pagetree)))


(define+provide/contract (next pnish [pt-or-path (current-pagetree)])
  (((or/c #f pagenodeish?)) ((or/c pagetree? pathish?)) . ->* . (or/c #f pagenode?))
  (match (next* pnish pt-or-path)
    [(list result _ ...) result]
    [else #f]))


(module-test-external
 (define test-pagetree `(pagetree-main foo bar (one (two three))))
 (check-equal? (next 'foo test-pagetree) 'bar)
 (check-equal? (next 'one test-pagetree) 'two)
 (check-false (next 'three test-pagetree)))


(define/contract+provide (path->pagenode path [starting-path (current-project-root)])
  ((coerce/path?) (coerce/path?) . ->* . coerce/symbol?)
  (define starting-dir
    (if (directory-exists? starting-path)
        starting-path
        (dirname starting-path)))
  (->output-path (find-relative-path (->complete-path starting-dir) (->complete-path path))))


(define+provide/contract (in-pagetree? pnish [pt-or-path (current-pagetree)])
  (((or/c #f pagenodeish?)) ((or/c pagetree? pathish?)) . ->* . boolean?)
  (and pnish (memq pnish (pagetree->list (get-pagetree pt-or-path))) #t))