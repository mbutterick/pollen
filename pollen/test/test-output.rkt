#lang racket/base
(require rackunit pollen/private/external/output racket/port)

(define-syntax-rule (check-output outputter string)
  (check-equal? (with-output-to-string (λ () outputter)) string))

;; output function should splice lists, and remove splicing char at the beginning of a list
(check-output (output '("tic" "tac" "toe")) "tictactoe")
(check-output (output '("tic" ("tac") "toe")) "tictactoe")
(check-output (output '("tic" (@ "tac") "toe")) "tictactoe")