#lang racket

(define places '(home-dir pref-dir pref-file temp-dir init-dir init-file links-file addon-dir doc-dir desk-dir sys-dir exec-file run-file collects-dir orig-dir))

(displayln (string-join (map (λ(x) (format "~a: ~a" x (find-system-path x))) places) "\n") )

(displayln (format "current-directory: ~a" (current-directory)))