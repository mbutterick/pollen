#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         version/utils
         scribble/base
         scribble/core
         scribble/private/manual-sprop
         scribble/private/manual-ex
         scribble/private/manual-style)

#|
Need this to make pollen docs buildable on v6.0.
`history` not added to scribble/manul till v6.1.
|#

(provide pollen-history)

(struct history-entry (what vers vers-stx expl))

(begin-for-syntax
 (define-splicing-syntax-class clause
   #:attributes (e)
   [pattern (~seq #:added vers)
            #:attr e #'(history-entry "Added" vers (quote-syntax vers) '("."))]
   [pattern (~seq #:changed vers content)
            #:attr e #'(history-entry "Changed" vers (quote-syntax vers)
                                      (list ": " content))]))

(define-syntax (pollen-history stx)
  (syntax-parse stx
    [(_ c:clause ...)
     #'(make-history (list c.e ...))]))

(define (make-history es)
  (for ([e (in-list es)])
    (define vers (history-entry-vers e))
    (unless (valid-version? vers)
      (raise-syntax-error 'history
                          (format "not a valid version: ~e"
                                  vers)
                          (history-entry-vers-stx e))))
  (delayed-block
   (lambda (renderer p ri)
     (define pkg
       (let ([from (resolve-get/tentative p ri '(exporting-packages #f))])
         (and from
              (pair? from)
              (car from))))
     (para
      #:style (style "SHistory" (list scheme-properties))
      (for/list ([e (in-list (sort es (lambda (a b) (version<? a b))
                                   #:key history-entry-vers))]
                 [i (in-naturals)])
        (define vers (history-entry-vers e))
        (list (if (zero? i)
                  null
                  (list null ; needed to avoid " " dropped as whitespace
                        " "))
              (history-entry-what e)
              " in version "
              vers
              (if (and pkg (zero? i))
                  (list " of package " (tt pkg))
                  null)
              (history-entry-expl e)))))))
