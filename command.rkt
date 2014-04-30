#lang racket/base
(require pollen/world)
(provide (all-defined-out))

(define (handle-test)
  `(displayln "raco pollen is installed correctly"))

(define (handle-help)
  `(displayln (format "Pollen commands:
help                  show this message
start  [dir] [port]   starts project server in dir (default is current dir) 
                          (default port is ~a)
render [dir]          render project in dir (default is current dir)
render path           render file
clone                 copy project to desktop without source files" ,(world:current-server-port))))

(define (handle-render dir-or-path rest-args)  
  `(begin 
     (require pollen/render pollen/world pollen/file sugar)
     (parameterize ([current-directory (world:current-project-root)])
       (define dir-or-path ,dir-or-path)
       (apply render-batch (map ->complete-path (if (not (directory-exists? dir-or-path))
                                                    (begin
                                                      (displayln (format "Rendering ~a" dir-or-path)) 
                                                      (cons dir-or-path ',rest-args))
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


(define (handle-clone directory rest-args)
  (define target-path (or 
                       (and rest-args (not (null? rest-args)) (path->complete-path (string->path (car rest-args))))
                       (build-path (find-system-path 'desk-dir) (string->path world:clone-directory-name))))
  
  `(begin
     (displayln "Cloning ...")
     (require racket/file pollen/file)
     (define (delete-it path)
       (cond
         [(directory-exists? path) (delete-directory/files path)]
         [(file-exists? path) (delete-file path)]))
     (define source-dir ,directory)
     (when (not (directory-exists? source-dir)) (error (format "clone error: source directory ~a does not exist" source-dir)))
     (define target-dir ,target-path)
     (when (directory-exists? target-dir) (delete-directory/files target-dir))
     (copy-directory/files source-dir target-dir)
     (for-each delete-it (find-files pollen-related-file? target-dir))
     (displayln (format "Completed to ~a" ,target-path))))

(define (handle-else command)
  `(if (regexp-match #rx"(shit|fuck)" ,command)
       (displayln (let ([responses '("Cursing at free software? Really?" "How uncouth." "Same to you, buddy.")])
                    (list-ref responses (random (length responses)))))
       (displayln (format "unknown command ~a" ,command))))
