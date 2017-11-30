#lang racket/base
(require pollen/setup scribble/reader racket/pretty version/utils racket/port racket/string)  
(provide (all-defined-out))

(define current-top-path (make-parameter #f))

(define (show doc parser-mode here-path)
  ;; we only want the top doc to print in the runtime environment
  ;; otherwise if a Pollen source imports others, they will all print their docs in sequence.
  ;; so only print if the current here-path is the top path, which is stored in the `current-top-path` parameter.
  (let ([ctp (current-top-path)])
    (when (and ctp (equal? here-path ctp))
      (if (memq parser-mode (list default-mode-preproc default-mode-template))
          (display doc)
          ;; #:newline option for `pretty-print` was introduced in 6.6.0.3,
          ;; so trim trailing newline manually
          (let ([pretty-print-proc (if (version<? (version) "6.7")
                                       (λ (x) (display (string-trim #:left? #f (with-output-to-string (λ () (pretty-print x))) "\n")))
                                       (λ (x) (pretty-print #:newline? #f x)))])
            ;; OK to use dynamic-require because runtime-config itself is dynamic-required
            (pretty-print-proc (with-handlers ([exn:fail? (λ (exn) ((error '|pollen markup error|
                                                                           ((dynamic-require 'racket/string 'string-join) (cdr ((dynamic-require 'racket/string 'string-split) (exn-message exn) ": ")) ": "))))])
                                 ((dynamic-require 'txexpr/base 'validate-txexpr) doc))))))))

(define (configure top-here-path)
  (current-top-path top-here-path)  ;; puts `show` into the right mode

  ;; wrap REPL interactions with pollen expression support
  (define old-read (current-read-interaction))
  (define pollen-readtable (make-at-readtable #:command-char (setup:command-char)))
  (define (new-read src in)
    (parameterize ([current-readtable pollen-readtable])
      (old-read src in)))
  (current-read-interaction new-read))

