#lang racket/base
(require racket/list racket/set)
(require "readability.rkt" "file-tools.rkt" "world.rkt")


(define (pd which) 
  (->path (format "/Users/MB/git/~a" which)))


(define (ineligible-path? f)
  (or (not (visible? f)) (member f RESERVED_PATHS)))

(define (unique-members xs)
  (set->list (list->set xs)))

(define (route-index [dir pollen-project-directory])
  (define unique-eligible-paths 
    (unique-members (map ->output-path
                         (filter-not ineligible-path? (directory-list dir)))))
  unique-eligible-paths)

(route-index (pd "foobar"))