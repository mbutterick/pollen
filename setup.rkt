#lang racket/base
(require racket/system racket/port racket/file racket/string racket/date)
(require "debug.rkt" "readability.rkt")

(provide setup)

(define (make-polcom-data racket-path)
  (format "#! ~a
#lang racket/base
(require pollen/command)
; pollen setup run on ~a" 
          (string-trim racket-path)
          (format "~a at ~a" (make-datestamp) (make-timestamp))))

(define (setup)
  (define cd (current-directory))
  (message (format "Setting up ~a as a pollen directory" cd))
  
  (define racket-path (with-output-to-string (λ() (system "which racket"))))
  (define path-to-racket-exists? (> (len racket-path) 0)) 
  
  (if path-to-racket-exists?
      (let ([polcom-data (make-polcom-data racket-path)]
            [polcom-filename "polcom"])
        (when (file-exists? polcom-filename)
          (begin
            (message (format "Deleting existing polcom file in ~a" cd))
            (delete-file polcom-filename)))
        (message (format "Creating new polcom file in ~a" cd))
        (with-handlers ([exn:fail? (λ(e) (message "Couldn't write polcom file. Aborting setup"))])
          (display-to-file polcom-data polcom-filename)
          (with-output-to-string (λ() (system (format "chmod 755 ~a" polcom-filename))))
          (message "Setup complete")
          (message (format "Run './~a start' to start pollen" polcom-filename))
          (message (format "Or run './~a help' for a list of commands" polcom-filename))
          (exit)))
      (message "No path to racket binary. Aborting setup")))