#lang racket/base
(require racket/path racket/list)
(require "file.rkt" "world.rkt" "decode.rkt" sugar txexpr "cache.rkt")


(define+provide current-pagetree (make-parameter #f))


(define+provide (pagenode? x)
  (->boolean (and (symbol? x) (with-handlers ([exn:fail? (λ(e) #f)])
                                (not (whitespace/nbsp? (->string x)))))))

(module-test-external
 (check-false (pagenode? "foo-bar"))
 (check-false (pagenode? "Foo_Bar_0123"))
 (check-true (pagenode? 'foo-bar))
 (check-false (pagenode? "foo-bar.p"))
 (check-false (pagenode? "/Users/MB/foo-bar"))
 (check-false (pagenode? #f))
 (check-false (pagenode? ""))
 (check-false (pagenode? " ")))


(define+provide (pagenodes? x)
  (and (list? x) (andmap pagenode? x)))


(define+provide (pagenodeish? x)
  (with-handlers ([exn:fail? (λ(e) #f)]) 
    (pagenode? (->symbol x))))


(define/contract+provide (->pagenode x)
  (pagenodeish? . -> . pagenode?)
  (->symbol x))


(define+provide/contract (decode-pagetree xs)
  (txexpr-elements? . -> . any/c) ; because pagetree is being explicitly validated
  (define pt-root-tag (world:current-pagetree-root-node))
  (define (splice-nested-pagetree xs)
    (apply append (for/list ([x (in-list xs)])
                            (if (and (txexpr? x) (eq? (get-tag x) pt-root-tag))
                                (get-elements x)
                                (list x)))))
  (validate-pagetree 
   (decode (cons pt-root-tag xs)
           #:txexpr-elements-proc (compose1 splice-nested-pagetree (λ(xs) (filter-not whitespace? xs))) 
           #:string-proc string->symbol))) ; because faster than ->pagenode


(define+provide (validate-pagetree x)
  (and (txexpr? x)
       (let ([pagenodes (pagetree->list x)])
         (and
          (andmap (λ(p) (or (pagenode? p) (error (format "validate-pagetree: \"~a\" is not a valid pagenode" p)))) pagenodes)
          (with-handlers ([exn:fail? (λ(e) (error (format "validate-pagetree: ~a" (exn-message e))))])
            (members-unique?/error pagenodes))
          x))))



(define+provide (pagetree? x)
  (with-handlers ([exn:fail? (λ(e) #f)]) 
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
    (define all-paths (filter visible? (remove-duplicates output-paths)))
    (define path-is-directory? (λ(f) (directory-exists? (build-path dir f))))
    (define-values (subdirectories files) (partition path-is-directory? all-paths))
    (define-values (pagetree-sources other-files) (partition pagetree-source? files))
    (define (sort-names xs) (sort xs #:key ->string string<?))
    ;; put subdirs in list ahead of files (so they appear at the top)
    (append (sort-names subdirectories) (sort-names pagetree-sources) (sort-names other-files)))
  
  ;; in general we don't filter the directory list for the automatic pagetree.
  ;; this can be annoying sometimes but it's consistent with the policy of avoiding magic behavior.
  ;; certain files (leading dot) will be ignored by `directory-list` anyhow.
  ;; we will, however, ignore Pollen's cache files, because those shouldn't be project-manipulated.
  (define (not-pollen-cache? path)
    (not (member (->string path) world:cache-names)))
  
  (if (directory-exists? dir )
      (decode-pagetree (map ->symbol (unique-sorted-output-paths (filter not-pollen-cache? (directory-list dir)))))
      (error (format "directory->pagetree: directory ~a doesn't exist" dir)))) 


(define+provide/contract (load-pagetree source-path)
  (pathish? . -> . pagetree?)
  (cached-require source-path (world:current-main-export)))


;; Try loading from pagetree file, or failing that, synthesize pagetree.
(define+provide/contract (make-project-pagetree project-dir)
  (pathish? . -> . pagetree?)
  (with-handlers ([exn:fail? (λ(exn) (directory->pagetree project-dir))])
    (define pagetree-source (build-path project-dir (world:current-default-pagetree)))
    (load-pagetree pagetree-source)))


(define+provide/contract (parent pnish [pt (current-pagetree)] #:allow-root [allow-root? #f])
  (((or/c #f pagenodeish?)) (pagetree? #:allow-root boolean?) . ->* . (or/c #f pagenode?))
  (define subtree? list?)
  (define (topmost-node x) (if (subtree? x) (car x) x))
  (define result
    (and pt pnish
         (let loop ([pagenode (->pagenode pnish)][subtree pt])
           (define current-parent (car subtree))
           (define current-children (cdr subtree))
           (if (member pagenode (map topmost-node current-children))
               current-parent
               (ormap (λ(st) (loop pagenode st)) (filter subtree? current-children))))))
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


(define+provide/contract (children p [pt (current-pagetree)])
  (((or/c #f pagenodeish?)) (pagetree?) . ->* . (or/c #f pagenodes?))  
  (and pt p 
       (let ([pagenode (->pagenode p)])
         (if (equal? pagenode (car pt))
             (map (λ(x) (if (list? x) (car x) x)) (cdr pt))
             (ormap (λ(x) (children pagenode x)) (filter list? pt))))))

(module-test-external
 (define test-pagetree `(pagetree-main foo bar (one (two three))))
 (check-equal? (children 'one test-pagetree) '(two))
 (check-equal? (children 'two test-pagetree) '(three))
 (check-false (children 'three test-pagetree))
 (check-false (children #f test-pagetree))
 (check-false (children 'fooburger test-pagetree)))


(define+provide/contract (siblings pnish [pt (current-pagetree)])
  (((or/c #f pagenodeish?)) (pagetree?) . ->* . (or/c #f pagenodes?))  
  (children (parent #:allow-root #t pnish pt) pt))

(module-test-external
 (define test-pagetree `(pagetree-main foo bar (one (two three))))
 (check-equal? (siblings 'one test-pagetree) '(foo bar one))
 (check-equal? (siblings 'foo test-pagetree) '(foo bar one))
 (check-equal? (siblings 'two test-pagetree) '(two))
 (check-false (siblings #f test-pagetree))
 (check-false (siblings 'invalid-key test-pagetree)))


;; flatten tree to sequence
(define+provide/contract (pagetree->list pt)
  (pagetree? . -> . pagenodes?)
  ; use cdr to get rid of root tag at front
  (cdr (flatten pt))) 

(module-test-external
 (define test-pagetree `(pagetree-main foo bar (one (two three))))
 (check-equal? (pagetree->list test-pagetree) '(foo bar one two three)))


(define (adjacents side pnish [pt (current-pagetree)])
  #;(symbol? pagenodeish? pagetree? . -> . pagenodes?)
  (and pt pnish
       (let* ([pagenode (->pagenode pnish)]
              [proc (if (equal? side 'left) takef takef-right)]
              [pagetree-nodes (pagetree->list pt)]
              [in-tree? (member pagenode pagetree-nodes)]
              [result (and in-tree? (proc pagetree-nodes (λ(x) (not (equal? pagenode x)))))])
         (and (not (empty? result)) result))))

(module-test-internal
 (require rackunit)
 (check-equal? (adjacents 'right 'one '(pagetree-index one two three)) '(two three))
 (check-false (adjacents 'right 'node-not-in-pagetree '(pagetree-index one two three))))


(define+provide/contract (previous* pnish [pt (current-pagetree)]) 
  (((or/c #f pagenodeish?)) (pagetree?) . ->* . (or/c #f pagenodes?))
  (adjacents 'left pnish pt))

(module-test-external
 (define test-pagetree `(pagetree-main foo bar (one (two three))))
 (check-equal? (previous* 'one test-pagetree) '(foo bar))
 (check-equal? (previous* 'three test-pagetree) '(foo bar one two))
 (check-false (previous* 'foo test-pagetree)))


(define+provide/contract (next* pnish [pt (current-pagetree)]) 
  (((or/c #f pagenodeish?)) (pagetree?) . ->* . (or/c #f pagenodes?))
  (adjacents 'right pnish pt))


(define+provide/contract (previous pnish [pt (current-pagetree)])
  (((or/c #f pagenodeish?)) (pagetree?) . ->* . (or/c #f pagenode?))
  (let ([result (previous* pnish pt)])
    (and result (last result))))

(module-test-external
 (define test-pagetree `(pagetree-main foo bar (one (two three))))
 (check-equal? (previous 'one test-pagetree) 'bar)
 (check-equal? (previous 'three test-pagetree) 'two)
 (check-false (previous 'foo test-pagetree)))




(define+provide/contract (next pnish [pt (current-pagetree)])
  (((or/c #f pagenodeish?)) (pagetree?) . ->* . (or/c #f pagenode?))
  (let ([result (next* pnish pt)])
    (and result (first result))))

(module-test-external
 (define test-pagetree `(pagetree-main foo bar (one (two three))))
 (check-equal? (next 'foo test-pagetree) 'bar)
 (check-equal? (next 'one test-pagetree) 'two)
 (check-false (next 'three test-pagetree)))


(define/contract+provide (path->pagenode path [starting-path (world:current-project-root)])
  ((coerce/path?) (coerce/path?) . ->* . coerce/symbol?)
  (define starting-dir
    (if (directory-exists? starting-path)
        starting-path
        (get-enclosing-dir starting-path)))
  (->output-path (find-relative-path (->complete-path starting-dir) (->complete-path path))))


(define+provide/contract (in-pagetree? pnish [pt (current-pagetree)])
  (((or/c #f pagenodeish?)) (pagetree?) . ->* . boolean?)
  (->boolean (and pnish (member pnish (pagetree->list pt)))))