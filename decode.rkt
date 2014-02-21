#lang racket/base
(require racket/contract xml txexpr)
(require "decode/fast.rkt" "predicates.rkt" "decode/typography-fast.rkt")

(provide to-string (contract-out [register-block-tag (symbol? . -> . void?)]
                       [decode ((xexpr/c) ;; use xexpr/c for contract on nx because it gives better error messages
                                
                                ;; todo: how to write more specific contracts for these procedures?
                                ;; e.g., string-proc should be restricted to procs that accept a string as input
                                ;; and return a string as output
                                (#:exclude-xexpr-tags list?
                                                      #:xexpr-tag-proc procedure?
                                                      #:xexpr-attrs-proc procedure?
                                                      #:xexpr-elements-proc procedure?
                                                      #:block-xexpr-proc procedure?
                                                      #:inline-xexpr-proc procedure?
                                                      #:string-proc procedure?)
                                . ->* . txexpr?)]
                       
                       [typogrify (string? . -> . string?)]
                       [nonbreaking-last-space ((txexpr?) (#:nbsp string? #:minimum-word-length integer?) . ->* . txexpr?)]
                       [wrap-hanging-quotes ((txexpr?) (#:single-prepend list? #:double-prepend list?) . ->* . txexpr?)]
                       [convert-linebreaks ((txexpr-elements?) (#:newline string?) . ->* . txexpr-elements?)]
                       [whitespace? (any/c . -> . boolean?)]
                       [paragraph-break? ((any/c) (#:pattern pregexp?) . ->* . boolean?)]
                       [merge-newlines (list? . -> . list?)]
                       [prep-paragraph-flow (txexpr-elements? . -> . txexpr-elements?)]
                       [wrap-paragraph ((txexpr-elements?) (#:tag symbol?) . ->* . block-xexpr?)]
                       [detect-paragraphs (txexpr-elements? . -> . txexpr-elements?)]
                       ))