#lang at-exp racket/base
(require rackunit racket/port racket/system racket/runtime-path compiler/find-exe pollen/setup)

;; define-runtime-path only allowed at top level
(define-runtime-path override-dir "data/override")
(define-runtime-path test.ptree "data/override/test.ptree")
(define-runtime-path test.html.pm "data/override/test.html.pm")
(define-runtime-path test.html.pmd "data/override/test.html.pmd")
(define-runtime-path test.html.pp "data/override/test.html.pp")

(define-runtime-path test.ptreeover "data/override/test.ptreeover")
(define-runtime-path test.html.pmover "data/override/test.html.pmover")
(define-runtime-path test.html.pmdover "data/override/test.html.pmdover")
(define-runtime-path test.html.ppover "data/override/test.html.ppover")
(define-runtime-path test-cmd.html.ppover "data/override/test-cmd.html.ppover")
(define-runtime-path test-exports.html.ppover "data/override/test-exports.html.ppover")
(define-runtime-path test-require.html.pmover "data/override/test-require.html.pmover")


;; `find-exe` avoids reliance on $PATH of the host system
(define racket-path (find-exe))
;; parameterize needed to pick up override file
(parameterize ([current-directory override-dir]
               [current-project-root override-dir])
  (when racket-path
    (define (run path)
      (define cmd-string (format "'~a' ~a" racket-path path))
      (with-output-to-string (λ () (system cmd-string))))
    ;; raco is in same dir as racket
    (define path-to-raco (path->string (simplify-path (build-path (find-exe) 'up "raco"))))
    ;; files with ordinary extensions will not be recognized in override dir, and thus behave like preproc
    (check-equal? (run test.ptree) "test\n====")
    (check-equal? (run test.html.pm) "test\n====")
    (check-equal? (run test.html.pmd) "test\n====")
    (check-equal? (run test.html.pp) "test\n====")
    
    (check-equal? (run test.ptreeover) "'(pagetree-root test ====)")
    (check-equal? (run test.html.pmover) "'(rootover \"test\" \"\\n\" \"====\")")
    (check-equal? (run test.html.pmdover) "'(rootover (h1 ((id \"test\")) \"test\"))")
    (check-equal? (run test.html.ppover) "test\n====")
(check-equal? (run test-cmd.html.ppover) "2")
    (check-equal? (dynamic-require test-exports.html.ppover 'docover) "2")
    (check-equal? (hash-ref (dynamic-require test-exports.html.ppover 'metasover) 'dog) "Roxy")
    (check-equal? (dynamic-require test-require.html.pmover 'docover) '(rootover "foobar"))))
