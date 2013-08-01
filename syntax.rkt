#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Macro that generates all the little xexpr functions
;; For each tag.
;;

(require (for-syntax racket/syntax))
(define-syntax (define-tags stx)
  (syntax-case stx ()
    [(_ name '(tags ...)) ; match pattern of the calling form
     #`(begin ; start with quasiquoted begin block & splice into it
         (define name '(tags ...)) ; assign the provided name to the tags as a group
         #,@(for/list ([tag (syntax->list #'(tags ...))]) ; step through list of tags
              (with-syntax ((tag-as-id (format-id stx "~a" tag))) ; convert tag into identifier
                ; todo: edit this to use tools:tagger
                #`(define (tag-as-id . x) `(tag-as-id ,@x)))))])) ; write out the xexpr function



(provide (all-defined-out))