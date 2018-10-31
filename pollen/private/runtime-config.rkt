#lang racket/base
(require pollen/setup
         scribble/reader
         racket/pretty
         version/utils
         racket/port
         racket/string
         txexpr/base)

(provide show configure current-top-path)

(define current-top-path (make-parameter #f))

(define (my-pretty-print x)
  ;; #:newline option for `pretty-print` was introduced in 6.6.0.3
  (if (version<? (version) "6.7")
      ;; so trim trailing newline manually in earlier versions
      (display (string-trim #:left? #f (with-output-to-string (λ () (pretty-print x))) "\n"))
      (pretty-print #:newline? #f x)))

(define (my-error-handler exn)
  (error '|pollen markup error| (string-join (cdr (string-split (exn-message exn) ": ")) ": ")))

(define (show doc parser-mode here-path)
  ;; we only want the top doc to print in the runtime environment
  ;; otherwise if a Pollen source imports others, they will all print their docs in sequence.
  ;; so only print if the current here-path is the top path, which is stored in the `current-top-path` parameter.
  (when (equal? here-path (current-top-path))
    (if (memq parser-mode (list default-mode-preproc default-mode-template))
        (display doc)
        (with-handlers ([exn:fail? my-error-handler])
          (my-pretty-print (validate-txexpr doc))))))

(define (configure top-here-path)
  (current-top-path top-here-path) ; puts `show` into the right mode
  (define old-read (current-read-interaction))
  (define (pollen-repl-read src in)
    ;; wrap repl interactions with pollen expression support
    (define pollen-readtable (make-at-readtable #:command-char (setup:command-char)))
    (parameterize ([current-readtable pollen-readtable])
      (old-read src in)))
  (current-read-interaction pollen-repl-read))

