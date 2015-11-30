#lang racket/base

(require racket/runtime-path
         racket/gui/base
         racket/class)

(provide make-drracket-buttons)

(module test racket/base) ; suppress testing by `raco test`

(define-runtime-path html-png-path "server-extras/cmd-char.png")

#| for toolbar callbacks, see

http://pkg-build.racket-lang.org/doc/tools/drracket_module-language-tools.html#%28elem._drracket~3atoolbar-buttons%29

|#

(define (make-command-char-button command-char)
  (let ([label (format "Insert command char ~a" command-char)]
        [bitmap (make-object bitmap% html-png-path 'png/mask)]
        [callback (λ (drr-frame)
                    (define editor (send drr-frame get-definitions-text))
                    (send editor begin-edit-sequence)
                    (send editor insert command-char)
                    (send editor end-edit-sequence))]
        [number 99])
    (list label bitmap callback number)))

(define (make-drracket-buttons command-char)
  (let ([command-char-button (make-command-char-button command-char)])
    (list command-char-button)))
