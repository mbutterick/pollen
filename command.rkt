#lang racket/base
(require (for-syntax racket/base))

;; Handle commands from raco

(define-for-syntax args (current-command-line-arguments))
(define-for-syntax arg-command-name (with-handlers ([exn:fail? (λ(exn) #f)]) (vector-ref args 0)))

(define-for-syntax arg-project-directory
  (with-handlers ([exn:fail? (λ(exn) (current-directory))])
    (path->complete-path (simplify-path (string->path (vector-ref args 1))))))

(define-for-syntax (command-error error-string)
  `(displayln (string-append "Error: ", error-string)))

;; we work in syntax layer because requiring pollen/server is slow.
(define-syntax (just-a-hook-for-the-macro stx)
  (datum->syntax stx 
                 (case arg-command-name
                   [(#f "help") ' (displayln "Pollen commands:
start       starts project server
render      renders all files in project directory
clone       copies rendered files to desktop
[filename]  renders individual file")]
                   [("start")
                    (if (not (directory-exists? arg-project-directory))
                        (command-error (format "~a is not a directory" arg-project-directory))
                        `(begin 
                           (require pollen/server pollen/world)
                           (parameterize ([world:current-project-root ,arg-project-directory])
                             (start-server))))]
                   [else (if (regexp-match #rx"(shit|fuck)" arg-command-name)
                             (displayln (let ([responses '("Cursing at free software? Really?" "How uncouth." "Same to you, buddy.")])
                                          (list-ref responses (random (length responses)))))
                             (command-error (format "unknown command ~a" arg-command-name)))])))

(just-a-hook-for-the-macro)



#|
                     [("render") `(begin
                                        ;; todo: take extensions off the comand line
                                        (displayln "Render preproc & ptree files ...")
                                        (require "render.rkt" "file-tools.rkt" "world.rkt")
                                        (apply render-batch (append-map project-files-with-ext (list world:preproc-source-ext world:ptree-source-ext))))]
                     [("clone") (let ([target-path 
                                       (if (> (len args) 1)
                                           (->path (get args 1))
                                           (build-path (find-system-path 'desk-dir) (->path "clone")))])
                                  `(begin
                                     (displayln "Clone & prune ...")
                                     (require racket/file)
                                     (require "tools.rkt")
                                     
                                     (define (pollen-related-file? file)
                                       (ormap (λ(proc) (proc file)) (list
                                                                     markup-source? 
                                                                     preproc-source? 
                                                                     template-source?
                                                                     ptree-source?
                                                                     pollen-script?
                                                                     magic-directory?
                                                                     racket-file?)))
                                     
                                     (define (delete-it path)
                                       (when (directory-exists? path)
                                         (delete-directory/files path))
                                       (when (file-exists? path)
                                         (delete-file path)))
                                     
                                     (let ([source-dir (current-directory)]
                                           [target-dir ,target-path])
                                       (when (directory-exists? target-dir)
                                         (delete-directory/files target-dir))
                                       (copy-directory/files source-dir target-dir)
                                       (map delete-it (find-files pollen-related-file? target-dir))
                                       (displayln (format "Completed to ~a" ,target-path))
                                       )))]
                     [("") `(displayln "No command given")]
                     ;; treat other input as a possible file name for rendering
                     [else (let ([possible-file (->path arg)])
                             (if (file-exists? possible-file)
                                 `(begin
                                    (require pollen/render)
                                    (render ,possible-file))
                                 `(displayln (format "No command defined for '~a'" ,arg))))]))))

(handle-pollen-command)

|#