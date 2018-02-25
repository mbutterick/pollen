#lang racket/base
(require (for-syntax racket/base racket/syntax syntax/strip-context "../setup.rkt" "split-metas.rkt")
         "to-string.rkt" "../pagetree.rkt" "splice.rkt" "../setup.rkt" "../core.rkt"
         (prefix-in doclang: "doclang-raw.rkt"))
(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [dialect-module-begin #%module-begin]))

;; PARSER-MODE-FROM-READER is used when Pollen is invoked as a specific dialect #lang, and the expander is the generic `pollen/main`.
;; PARSER-MODE-FROM-EXPANDER is used when Pollen is invoked through an inline submodule,
;; and the expander is a specific dialect: `pollen/markup`, `pollen/pre`, etc.
;; they only differ in the `pollen/main` dialect, where the reader infers the dialect from the file extension.

(define-syntax-rule (dialect-module-begin PARSER-MODE-FROM-EXPANDER READER-SUBMOD)
  (#%module-begin
   (require racket/base)
   (provide (except-out (all-from-out racket/base) #%module-begin)
            (rename-out [pmb #%module-begin]))
   (define-syntax (pmb stx)
     (syntax-case stx ()
       [(_ . EXPRS) (with-syntax ([EXPRS (syntax-property #'EXPRS 'parser-mode-from-expander 'PARSER-MODE-FROM-EXPANDER)])
                      #'(pollen-module-begin . EXPRS))]))
   READER-SUBMOD))


(define (make-parse-proc parser-mode root-proc)
  (define (stringify xs) (apply string-append (map to-string xs)))
  (cond
    [(eq? parser-mode default-mode-pagetree) decode-pagetree]
    [(eq? parser-mode default-mode-markup) (λ (xs) (apply root-proc (remove-voids xs)))] 
    [(eq? parser-mode default-mode-markdown)
     (λ (xs) (let* ([xs (stringify xs)]
                    [xs ((dynamic-require 'markdown 'parse-markdown) xs)]
                    [xs (map strip-empty-attrs xs)])
               (apply root-proc xs)))]
    [else stringify])) ; preprocessor mode


(define (strip-leading-newlines doc)
  ;; drop leading newlines, as they're often the result of `defines` and `requires`
  (or (memf (λ (ln) (and (not (equal? ln (setup:newline)))
                         (not (equal? ln "")))) doc) null))


(define-syntax (pollen-module-begin stx)
  (syntax-case stx ()
    [(_ . EXPRS)
     (with-syntax (;; 'parser-mode-from-reader will be #f for an inline submodule
                   [EXPRS (replace-context #'here #'EXPRS)]
                   [PARSER-MODE-FROM-READER (syntax-property stx 'parser-mode-from-reader)]
                   [PARSER-MODE-FROM-EXPANDER (syntax-property #'EXPRS 'parser-mode-from-expander)]
                   [META-HASH (split-metas #'EXPRS (setup:define-meta-name))]
                   [METAS-ID (setup:meta-export)]
                   [META-MOD-ID (setup:meta-export)]
                   [ROOT-ID (setup:main-root-node)]
                   [DOC-ID (setup:main-export)])
       #'(doclang:#%module-begin
          DOC-ID ; positional arg for doclang-raw: name of export
          (λ (xs)
            (define parser-mode (or 'PARSER-MODE-FROM-READER PARSER-MODE-FROM-EXPANDER))
            (define proc (make-parse-proc parser-mode ROOT-ID))
            (define doc-elements (splice (strip-leading-newlines xs) (setup:splicing-tag)))
            (proc doc-elements)) ;  positional arg for doclang-raw: post-processor
          (module META-MOD-ID racket/base
            (provide METAS-ID)
            (define METAS-ID META-HASH))
          (require pollen/top pollen/core pollen/setup (submod "." META-MOD-ID))
          (provide (all-defined-out) METAS-ID DOC-ID)
          (define prev-metas (current-metas))
          (and (current-metas METAS-ID) "") ; because empty strings get stripped, voids don't
          (begin . EXPRS)
          (and (current-metas prev-metas) "")))])) ; leave behind empty string, not void
