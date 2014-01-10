#lang racket/base
(require racket/system racket/port racket/file 
         racket/string racket/date racket/class racket/list)
(require "debug.rkt" "readability.rkt")

(provide setup)

(define cd (current-directory))
(define test-mode (make-parameter #f))

;; simple class for setup tasks
(define task% 
  (class object%
    (init do-proc [undo-proc #f])
    (define this-do do-proc)
    (define this-undo undo-proc)
    (super-new)
    (define/public (do-task) (this-do))
    (define/public (undo-task) (if this-undo (this-undo) (void)))))

;; function for invoking method of multiple objects
(define-syntax send-each
  (syntax-rules ()
    ;; invoke a method of every object in list
    [(send-each objects do-method)
     (for-each (λ(obj) (send obj do-method)) objects)]
    ;; invoke a method of every object in list, but
    ;; if error, invoke the undo method all the way backwards
    ;; allows structured cleanup
    [(send-each objects do-method undo-method)
     (begin
       (define (send-stack-inner objects-inner)
         (if (empty? objects-inner)
             (void)
             (with-handlers ([exn:fail? (λ(err) 
                                          (begin 
                                            (send (car objects-inner) undo-method) 
                                            (raise err)))])
               (send (car objects-inner) do-method)
               (send-stack-inner (cdr objects-inner)))))
       ;; error has been handled all the way back up the stack
       ;; so kill it here 
       (with-handlers ([exn:fail? (λ(err) (void))])
         (send-stack-inner objects)))]))



(define (make-polcom-data racket-path)
  (format "#! ~a
#lang racket/base
(require pollen/command)
; pollen setup run in ~a on ~a" 
          racket-path
          cd
          (format "~a at ~a" (make-datestamp) (make-timestamp))))


;; task: get confirmation
(define (get-confirmation)
  (message (format "Set up ~a as a pollen directory? ['y' or 'yes']" cd))
  (define setup-confirm (->string (read)))
  (when (not (ormap (λ(input)(equal? setup-confirm input)) (list "y" "yes")))
    (error))
  (message (format "Setting up ~a as a pollen directory" cd)))

(define (abort-confirmation)
  (message "Aborting setup"))

(define confirm-setup
  (new task% 
       [do-proc get-confirmation]
       [undo-proc abort-confirmation]))


;; task: make polcom file
(define polcom-filename "polcom")

(define (delete-polcom-file-if-existing)
  (when (file-exists? polcom-filename)
    (begin
      (message (format "Deleting existing polcom file in ~a" cd))
      (delete-file polcom-filename))))

(define (save-polcom-file)
  (define racket-path (string-trim (with-output-to-string (λ() (system "which racket")))))
  (define path-to-racket-exists? (> (len racket-path) 0)) 
  
  (if path-to-racket-exists?
      (let ([polcom-data (make-polcom-data racket-path)])
        (message (format "Using ~a as racket path" racket-path))
        (delete-polcom-file-if-existing)
        (message (format "Creating new polcom file in ~a" cd))
        (if (not (test-mode))
            (begin
              (display-to-file polcom-data polcom-filename)
              (with-output-to-string (λ() (system (format "chmod 755 ~a" polcom-filename)))))
            (message "[test mode: file would be saved now]")))
      (begin 
        (message "No path to Racket binary")
        (error))))

(define (abort-polcom-file)
  (message "Couldn't create polcom file")
  (delete-polcom-file-if-existing))

(define make-polcom
  (new task%
       [do-proc save-polcom-file]
       [undo-proc abort-polcom-file]))


;; task: report success

(define (success-messages)
  (message "Setup complete")
  (define path-to-polcom (format "~a~a" cd polcom-filename))
  (message (format "Run '~a start' to start project server" path-to-polcom))
  (message (format "Or run '~a help' for a list of commands" path-to-polcom))
  (when (not (test-mode)) (exit)))

(define report-success
  (new task% 
       [do-proc success-messages]))

(define (setup)
  (define tasks (list confirm-setup make-polcom report-success))
  (send-each tasks do-task undo-task))


;; better way to do this is to create separate removal tasks
;; (that may rely on some of the same intermediate functions)
;; so that errors can be caught on the way out too.
;; also, remove conflicts with existing racket name
;;(define (remove)
;;  (send-each (reverse tasks) remove-task))

(module+ main
  (parameterize ([test-mode #t])
    (setup)))