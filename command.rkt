#lang racket/base

(provide (all-defined-out))

(define (handle-help)
  `(displayln "Pollen commands:
start       starts project server
render      renders all files in project directory
clone       copies rendered files to desktop
[filename]  renders individual file"))

(define (handle-start directory)
  (if (not (directory-exists? directory))
      (error (format "~a is not a directory" directory))
      `(begin 
         (require pollen/server pollen/world)
         (parameterize ([world:current-project-root ,directory])
           (start-server)))))

(define (handle-else command)
  `(if (regexp-match #rx"(shit|fuck)" ,command)
       (displayln (let ([responses '("Cursing at free software? Really?" "How uncouth." "Same to you, buddy.")])
                    (list-ref responses (random (length responses)))))
       (displayln (format "unknown command ~a" ,command))))




#|
                     [("render") `(begin
                                        ;; todo: take extensions off the comand line
                                        (displayln "Render preproc & pagemap files ...")
                                        (require "render.rkt" "file-tools.rkt" "world.rkt")
                                        (apply render-batch (append-map project-files-with-ext (list world:preproc-source-ext world:pagemap-source-ext))))]
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
                                                                     pagemap-source?
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