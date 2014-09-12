#lang racket/base
(require (for-syntax racket/base pollen/command))

;; Handle commands from raco

(define-for-syntax args (current-command-line-arguments))
(define-for-syntax arg-command-name (with-handlers ([exn:fail? (λ(exn) #f)]) (vector-ref args 0)))


(define-for-syntax first-arg-or-current-dir
  (with-handlers ([exn:fail? (λ(exn) (current-directory))])
    ;; incoming path argument is handled as described in
    ;; docs for current-directory
    (path->complete-path (simplify-path (cleanse-path (string->path (vector-ref args 1)))))))

(define-for-syntax rest-args
  (with-handlers ([exn:fail? (λ(exn) #f)])
    (cddr (vector->list (current-command-line-arguments)))))

(define-for-syntax port-arg
  (with-handlers ([exn:fail? (λ(exn) #f)])
    (string->number (vector-ref args 2))))

(define-for-syntax (command-error error-string)
  `(displayln (string-append "Error: ", error-string)))

;; we work in syntax layer because 'start' has to require pollen/server,
;; which is slow, and needs to happen at the top level.
(define-syntax (select-syntax-for-command stx)
  (datum->syntax stx 
                 (case arg-command-name
                   [("test" "xyzzy") (handle-test)]
                   [(#f "help") (handle-help)]
                   [("start") (handle-start (path->directory-path first-arg-or-current-dir) port-arg)]
                   [("render") (handle-render first-arg-or-current-dir rest-args)]
                   [("clone") (handle-clone first-arg-or-current-dir rest-args)]
                   [else (handle-else arg-command-name)])))

(select-syntax-for-command)


