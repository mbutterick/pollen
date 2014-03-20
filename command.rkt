#lang racket/base
(require pollen/world)
(provide (all-defined-out))

(define (handle-help)
  `(displayln (format "Pollen commands:
help                  show this message
start  [dir] [port]   starts project server in dir (default is current dir) 
                          (default port is ~a)
render [dir]          render project in dir (default is current dir)
render path           render file
clone                 copy project to desktop without source files" ,(world:current-server-port))))

(define (handle-render dir-or-path [port #f])  
  `(begin 
     (require pollen/render pollen/world pollen/file sugar)
     (parameterize ([current-directory (world:current-project-root)])
       (define dir-or-path ,dir-or-path)
       (apply render-batch (map ->complete-path (if (not (directory-exists? dir-or-path))
                                                    (begin
                                                      (displayln (format "Rendering ~a" dir-or-path))                                                      
                                                      (list dir-or-path))
                                                    (begin
                                                      (displayln (format "Rendering preproc & pagetree files in directory ~a" dir-or-path))
                                                      (apply append (map (λ(proc) (filter proc (directory-list dir-or-path))) (list preproc-source? pagetree-source?))))))))))


(define (handle-start directory [port #f])
  (if (not (directory-exists? directory))
      (error (format "~a is not a directory" directory))
      `(begin 
         (require pollen/server pollen/world)
         (parameterize ([world:current-project-root ,directory]
                        ,@(if port (list `(world:current-server-port ,port)) null))
           (start-server)))))

(define (handle-else command)
  `(if (regexp-match #rx"(shit|fuck)" ,command)
       (displayln (let ([responses '("Cursing at free software? Really?" "How uncouth." "Same to you, buddy.")])
                    (list-ref responses (random (length responses)))))
       (displayln (format "unknown command ~a" ,command))))




#|
   
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
                                                                     pagetree-source?
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

|#