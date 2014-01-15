#lang racket/base
(require racket/list racket/set)
(require "readability.rkt" "file-tools.rkt" "world.rkt" "debug.rkt")


(define (pd which) 
  (->path (format "/Users/MB/git/pollen/~a" which)))


(define (route-index [dir pollen-project-directory])
  (define (make-link-cell [href+text (cons #f #f)])
    (define href (car href+text))
    (define text (cdr href+text))
    (filter-not void? `(td ,(when (and href text) 
                              `(a ((href ,href)) ,text)))))
  
  (define (make-path-row p)
    (define pstring (->string p))
    (define (file-in-dir? p) (file-exists? (apply build-path (map ->path (list dir p)))))
    (define sources (filter file-in-dir? (list (->preproc-source-path pstring) (->pollen-source-path pstring))))
    `(tr ,@(map make-link-cell (list 
                                (cons pstring pstring)
                                (cons (format "raw/~a" pstring) "raw")
                                (if (not (empty? sources))
                                    (cons (->string (car sources)) "source")
                                    (cons #f #f))))))
  
  (define (unique-sorted-paths xs)
    (sort (set->list (list->set (map ->output-path xs))) #:key ->string string<?))
  
  (define (ineligible-path? x) (or (not (visible? x)) (member x RESERVED_PATHS)))  
  
  (define project-paths (filter-not ineligible-path? (directory-list dir)))
  `(table ,@(map make-path-row (unique-sorted-paths project-paths))))

(route-index (pd "foobar"))