#lang racket/base
(require rackunit)
(require "../render.rkt")
(require/expose "../render.rkt" (modification-date-hash make-mod-dates-key path->mod-date-value store-render-in-modification-dates modification-date-expired?))

(check-pred hash? modification-date-hash)

(define sample-dir (string->path "samples"))
(define samples (parameterize ([current-directory sample-dir])
                  (map path->complete-path (directory-list "."))))
(define-values (sample-01 sample-02 sample-03) (apply values samples))

(check-equal? (make-mod-dates-key samples) samples)

(check-false (path->mod-date-value (path->complete-path "garbage-path.zzz")))
(check-equal? (path->mod-date-value sample-01) (file-or-directory-modify-seconds sample-01))

(check-equal? (store-render-in-modification-dates sample-01 sample-02 sample-03) (void))
(check-true (hash-has-key? modification-date-hash (list sample-01 sample-02 sample-03)))

(check-true (modification-date-expired? sample-01)) ; because key hasn't been stored
(check-false (apply modification-date-expired? samples)) ; because files weren't changed


