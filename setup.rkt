#lang racket/base
(require racket/system racket/port racket/file racket/string racket/date)
(require "debug.rkt" "readability.rkt")

(provide setup)

(define cd (current-directory))

(define (make-polcom-data racket-path)
  (format "#! ~a
#lang racket/base
(require pollen/command)
; pollen setup run in ~a on ~a" 
          racket-path
          cd
          (format "~a at ~a" (make-datestamp) (make-timestamp))))

(define (setup)
  ; this function is just for export.
  ; it hides the test-only parameter
  (setup-testable #f))

(define (setup-testable test-only)    
  (message (format "Set up ~a as a pollen directory? ['y' or 'yes']" cd))
  (define setup-confirm (->string (read)))
  (when (not (or (equal? setup-confirm "y") (equal? setup-confirm "yes")))
    (begin
      (message "Aborting setup")
      (exit)))
  (message (format "Setting up ~a as a pollen directory" cd))
  
  (define racket-path (string-trim (with-output-to-string (λ() (system "which racket")))))
  (define path-to-racket-exists? (> (len racket-path) 0)) 
  
  (if path-to-racket-exists?
      (let ([polcom-data (make-polcom-data racket-path)]
            [polcom-filename "polcom"])
        (message (format "Using ~a as racket path" racket-path))
        (when (file-exists? polcom-filename)
          (begin
            (message (format "Deleting existing polcom file in ~a" cd))
            (delete-file polcom-filename)))
        (with-handlers ([exn:fail? (λ(e) (message "Couldn't write polcom file. Aborting setup"))])
          (when (not test-only)
              (begin
                (message (format "Creating new polcom file in ~a" cd))
                (display-to-file polcom-data polcom-filename)
                (with-output-to-string (λ() (system (format "chmod 755 ~a" polcom-filename))))))
          (message "Setup complete")
          (message (format "Run '~a~a start' to start project server" cd polcom-filename))
          (message (format "Or run '~a~a help' for a list of commands" cd polcom-filename))
          (when (not test-only) (exit))))
      (message "No path to racket binary. Aborting setup")))

(module+ main
  (setup-testable #t))