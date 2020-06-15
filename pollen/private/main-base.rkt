#lang racket/base
(require (for-syntax racket/base
                     syntax/strip-context
                     "../setup.rkt"
                     "splice.rkt"
                     "split-metas.rkt")
         racket/list
         "to-string.rkt"
         "../pagetree.rkt"
         "splice.rkt"
         "../core.rkt"
         "../setup.rkt"
         (prefix-in doclang: "external/doclang-raw.rkt"))
(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [pollen-module-begin #%module-begin])
         (all-from-out "../core.rkt" "../setup.rkt"))

(define (strip-leading-newlines doc)
  ;; drop leading newlines, as they're often the result of `defines` and `requires`
  (if (setup:trim-whitespace?)
      (dropf doc (λ (ln) (member ln (list (setup:newline) ""))))
      doc))

(define (stringify xs) (apply string-append (map to-string xs)))

(define (parse xs-in parser-mode root-proc)
  (define xs (splice (strip-leading-newlines xs-in) pollen-splicing-tag))
  (cond
    [(eq? parser-mode default-mode-pagetree) (decode-pagetree xs)]
    [(eq? parser-mode default-mode-markup) (apply root-proc (remove-voids xs))] 
    [(eq? parser-mode default-mode-markdown)
     (let* ([xs (stringify xs)]
            [xs ((dynamic-require 'markdown 'parse-markdown) xs)]
            [xs (map strip-empty-attrs xs)])
       (apply root-proc xs))]
    [else (stringify xs)])) ; preprocessor mode

(define-syntax (pollen-module-begin stx)
  (syntax-case stx ()
    [(_ PARSER-MODE . EXPRS)
     (with-syntax ([META-HASH (split-metas #'EXPRS pollen-define-meta-name)]
                   [METAS-ID pollen-meta-export]
                   [METAS-ID-CALLER (datum->syntax #'EXPRS pollen-meta-export)]
                   [ROOT-ID (datum->syntax #'EXPRS (setup:main-root-node))]
                   [POLLEN/TOP (datum->syntax #'EXPRS 'pollen/top)]
                   [DOC-ID pollen-main-export]
                   [ALL-DEFINED-OUT (datum->syntax #'EXPRS '(all-defined-out))])
       #'(doclang:#%module-begin
          DOC-ID ; positional arg for doclang-raw: name of export
          (λ (xs) ; positional arg for doclang-raw: post-processor
            ;; wait till the end to restore prev-metas
            ;; because tag functions may edit current-metas
            ;; and we want root to see those changes
            (begin0
              (parse xs PARSER-MODE ROOT-ID)
              ;; pick up any imperative changes to current-metas by tag functions
              (set! METAS-ID-CALLER (current-metas))
              ;; restore previous value of metas
              (current-metas prev-metas))) 
          (module METAS-ID racket/base
            (provide METAS-ID)
            (define METAS-ID META-HASH))
          (require POLLEN/TOP (submod "." METAS-ID))
          (provide ALL-DEFINED-OUT; implicitly picks up METAS-ID-CALLER
                   DOC-ID)
          ;; we set current-metas imperatively rather than using `splicing-parameterize`
          ;; so that we can restore it in the post-processor, rather than out here
          (define METAS-ID-CALLER METAS-ID) ; grab the new metas
          (define prev-metas (current-metas)) ; stash the old metas
          (and (current-metas METAS-ID-CALLER) "") ; because empty strings get stripped, voids don't
          (begin . EXPRS)))])) 
