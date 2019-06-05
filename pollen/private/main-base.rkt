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
    [(== default-mode-pagetree) (decode-pagetree xs)]
    [(== default-mode-markup) (apply root-proc (remove-voids xs))] 
    [(== default-mode-markdown)
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
                   [DOC-ID (setup:main-export)])
       #'(doclang:#%module-begin
          DOC-ID ; positional arg for doclang-raw: name of export
          (λ (xs)
            (define proc (make-parse-proc PARSER-MODE ROOT-ID))
            (define trimmed-xs (strip-leading-newlines xs))
            (define doc-elements (splice trimmed-xs (setup:splicing-tag)))
            (parameterize ([current-metas METAS-ID-CALLER])
              (proc doc-elements))) ;  positional arg for doclang-raw: post-processor
          (module METAS-ID racket/base
            (provide METAS-ID)
            (define METAS-ID META-HASH))
          (require POLLEN/TOP (submod "." METAS-ID))
          (provide (all-defined-out) METAS-ID DOC-ID)
          (define prev-metas (current-metas))
          (define METAS-ID-CALLER METAS-ID)
          (and (current-metas METAS-ID) "") ; because empty strings get stripped, voids don't
          (begin . EXPRS)
          (and (current-metas prev-metas) "")))])) ; leave behind empty string, not void
