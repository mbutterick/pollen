#lang racket/base
(require (for-syntax racket/base
                     syntax/strip-context
                     "../setup.rkt"
                     "split-metas.rkt")
         racket/match
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

(define ((make-parse-proc parser-mode root-proc) xs)
  (define (stringify xs) (apply string-append (map to-string xs)))
  (match parser-mode
    [(== default-mode-pagetree eq?) (decode-pagetree xs)]
    [(== default-mode-markup eq?) (apply root-proc (remove-voids xs))] 
    [(== default-mode-markdown eq?)
     (let* ([xs (stringify xs)]
            [xs ((dynamic-require 'markdown 'parse-markdown) xs)]
            [xs (map strip-empty-attrs xs)])
       (apply root-proc xs))]
    [_ (stringify xs)])) ; preprocessor mode

(define (strip-leading-newlines doc)
  ;; drop leading newlines, as they're often the result of `defines` and `requires`
  (if (setup:trim-whitespace?)
      (dropf doc (λ (ln) (member ln (list (setup:newline) ""))))
      doc))

(define-syntax (pollen-module-begin stx)
  (syntax-case stx ()
    [(_ PARSER-MODE . EXPRS)
     (with-syntax ([META-HASH (split-metas #'EXPRS (setup:define-meta-name))]
                   [METAS-ID (setup:meta-export)]
                   [METAS-ID-CALLER (datum->syntax #'EXPRS (setup:meta-export))]
                   [ROOT-ID (datum->syntax #'EXPRS (setup:main-root-node))]
                   [POLLEN/TOP (datum->syntax #'EXPRS 'pollen/top)]
                   [DOC-ID (setup:main-export)]
                   [ALL-DEFINED-OUT (datum->syntax #'EXPRS '(all-defined-out))])
       #'(doclang:#%module-begin
          DOC-ID ; positional arg for doclang-raw: name of export
          (λ (xs) ;  positional arg for doclang-raw: post-processor
            (define proc (make-parse-proc PARSER-MODE ROOT-ID))
            (define trimmed-xs (strip-leading-newlines xs))
            (define doc-elements (splice trimmed-xs (setup:splicing-tag)))
            (begin0
              (proc doc-elements)
              ;; wait till the end to restore prev-metas
              ;; because tag functions may edit current-metas
              ;; and we want root to see those changes
              (current-metas prev-metas))) 
          (module METAS-ID racket/base
            (provide METAS-ID)
            (define METAS-ID META-HASH))
          (require POLLEN/TOP (submod "." METAS-ID))
          (provide ALL-DEFINED-OUT ; implicitly picks up METAS-ID-CALLER
                   DOC-ID)
          (define prev-metas (current-metas))
          (define METAS-ID-CALLER METAS-ID)
          (and (current-metas METAS-ID) "") ; because empty strings get stripped, voids don't
          ;; we set current-metas imperatively rather than using `splicing-parameterize`
          ;; so that we can unset it in the post processor, rather than out here
          (begin . EXPRS)))])) 
