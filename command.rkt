#lang racket/base
(require pollen/world sugar/coerce)
(provide (all-defined-out))

(define (very-nice-path x)
  (path->complete-path (simplify-path (cleanse-path (->path x)))))

(define (handle-test)
  `(displayln "raco pollen is installed correctly"))

(define (handle-help)
  `(displayln (format "Pollen commands:
help                  show this message
start  [dir] [port]   starts project server in dir (default is current dir) 
                          (default port is ~a)
render [dir] [dest]   render project in dir (default is current dir) 
                          to dest (default is desktop)
render filename       render filename only (can be source or output name)
clone                 copy project to desktop without source files" ,(world:current-server-port))))


(define (handle-render path-args)
  `(begin
     (require pollen/render pollen/world pollen/file sugar pollen/pagetree racket/list pollen/command racket/string)
     (parameterize ([current-directory (world:current-project-root)])
       (define path-args ',path-args)
       (define first-arg (car path-args))
       (if (directory-exists? first-arg)
           (let ([dir first-arg]) ; now we know it's a dir
             (parameterize ([current-directory dir]
                            [world:current-project-root dir])
               (define preprocs (filter preproc-source? (directory-list dir)))
               (define static-pagetrees (filter pagetree-source? (directory-list dir)))
               ;; if there are no static pagetrees, use make-project-pagetree
               ;; (which will synthesize a pagetree if needed, which includes all sources)
               (define preprocs-and-static-pagetrees (append preprocs static-pagetrees))
               (define batch-to-render
                 (map very-nice-path
                      (cond
                        [(empty? preprocs-and-static-pagetrees)
                         (displayln (format "Rendering generated pagetree for directory ~a" dir))
                         (cdr (make-project-pagetree dir))]
                        [else
                         (displayln (format "Rendering preproc & pagetree files in directory ~a" dir))
                         preprocs-and-static-pagetrees])))
               (apply render-batch batch-to-render)))
           (begin ; first arg is a file
             (displayln (format "Rendering ~a" (string-join (map ->string path-args) " "))) 
             (apply render-batch path-args))))))


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
     (require racket/file pollen/file racket/list)
     (define (delete-it path)
       (cond
         [(directory-exists? path) (delete-directory/files path)]
         [(file-exists? path) (delete-file path)]))
     (define (contains-directory? possible-superdir possible-subdir)
       (define (has-prefix? xs prefix)
         (and (>= (length xs) (length prefix))
              (andmap equal? prefix (take xs (length prefix)))))
       ((explode-path possible-subdir) . has-prefix? . (explode-path possible-superdir)))
     (define source-dir (simplify-path ,directory))
     (when (not (directory-exists? source-dir)) (error 'clone (format "source directory ~a does not exist" source-dir)))
     (define target-dir (simplify-path ,target-path))
     (when (source-dir . contains-directory? . target-dir) (error 'clone "aborted because target directory for cloning (~a) can't be inside source directory (~a)" target-dir source-dir))
     (displayln "Cloning ...")
     (when (directory-exists? target-dir) (delete-directory/files target-dir))
     (copy-directory/files source-dir target-dir)
     (for-each delete-it (find-files pollen-related-file? target-dir))
     (displayln (format "Completed to ~a" target-dir))))

(define (handle-else command)
  `(if (regexp-match #rx"(shit|fuck)" ,command)
       (displayln (let ([responses '("Cursing at free software? Really?" "How uncouth." "Same to you, buddy.")])
                    (list-ref responses (random (length responses)))))
       (displayln (format "unknown command ~a" ,command))))
