#lang racket/base
(require racket/contract racket/list)
(require tagged-xexpr sugar "debug.rkt" "predicates.rkt" "world.rkt")
(provide (all-from-out "debug.rkt" "predicates.rkt" racket/list))

;; setup for test cases
(module+ test (require rackunit))

;; list of all eligible requires in project require directory
(define+provide/contract (get-project-require-files)
  (-> (or/c #f (listof complete-path?)))
  (define extras-directory (build-path PROJECT_ROOT EXTRAS_DIR))
  (and (directory-exists? extras-directory)
       ;; #:build? option returns complete paths (instead of just file names)
       (let ([files (filter project-require-file? (directory-list extras-directory #:build? #t))])
         (and (not (empty? files)) files))))


;; convert list of meta tags to a hash for export from pollen document.
;; every meta is form (meta "key" "value") (enforced by contract)
;; later metas with the same name will override earlier ones.
(define+provide/contract (make-meta-hash mxs)
  ((listof meta-xexpr?) . -> . hash?)
  (apply hash (append-map tagged-xexpr-elements mxs)))

(module+ test
  (check-equal? (make-meta-hash '((meta "foo" "bar")(meta "hee" "haw")))
                (hash "foo" "bar" "hee" "haw"))
  (check-equal? (make-meta-hash '((meta "foo" "bar")(meta "foo" "haw")))
                (hash "foo" "haw")))


