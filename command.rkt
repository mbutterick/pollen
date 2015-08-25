#lang racket/base
(require pollen/world pollen/render racket/file racket/path sugar/coerce pollen/file pollen/pagetree racket/string racket/list)

;; The use of dynamic-require throughout this file is intentional:
;; this way, low-dependency raco commands (like "version") are faster.
;; Whereas with `require` or `local-require`, everything would have to be front-loaded.
;; but ... maybe most of the latency is due to pollen/world environment checking.
;; todo: investigate this

(module+ raco
  (define command-name (with-handlers ([exn:fail? (λ _ #f)])
                         (vector-ref (current-command-line-arguments) 0)))
  (dispatch command-name))

(define (dispatch command-name)
  (define (get-first-arg-or-current-dir)
    (normalize-path
     (with-handlers ([exn:fail? (λ(exn) (current-directory))])
       ;; incoming path argument is handled as described in docs for current-directory
       (very-nice-path (vector-ref (current-command-line-arguments) 1)))))
  (case command-name
    [("test" "xyzzy") (handle-test)]
    [(#f "help") (handle-help)]
    [("start") (define port-arg
                 (with-handlers ([exn:fail? (λ _ #f)])
                   (string->number (vector-ref (current-command-line-arguments) 2))))
               (handle-start (path->directory-path (get-first-arg-or-current-dir)) port-arg)]
    ;; "second" arg is actually third in command line args, so use cddr not cdr
    [("render") (handle-render (cons (get-first-arg-or-current-dir)
                                     (let ([clargs (vector->list (current-command-line-arguments))])
                                       (if (>= (length clargs) 3)
                                           (map very-nice-path (cddr clargs))
                                           null))))]
    [("version") (handle-version)]
    [("reset") (handle-reset)]
    [("clone" "publish") (define rest-args
                           (with-handlers ([exn:fail? (λ _ #f)])
                             (cddr (vector->list (current-command-line-arguments)))))
                         (handle-publish (get-first-arg-or-current-dir) rest-args command-name)]
    [else (handle-unknown command-name)]))

(define (very-nice-path x)
  (path->complete-path (simplify-path (cleanse-path (->path x)))))

(define (handle-test)
  (displayln "raco pollen is installed correctly"))

(define (handle-help)
  (displayln (format "Pollen commands:
help                   show this message
start   [dir] [port]   starts project server in dir (default is current dir) 
                          (default port is ~a)
render  [dir] [dest]   render project in dir (default is current dir) 
                          to dest (default is desktop)
render filename        render filename only (can be source or output name)
publish                copy project to desktop without source files
publish [dir] [dest]   copy project in dir to dest without source files
                          (warning: overwrites existing dest dir)
reset                  reset compile cache
version                print the version (~a)" (world:current-server-port) (world:current-pollen-version))))


(define (handle-version)
  (displayln (world:current-pollen-version)))

(define (handle-reset)
  (displayln "resetting cache ...")
  ((dynamic-require 'pollen/cache 'reset-cache)))


(define (handle-render path-args)
  (parameterize ([current-directory (world:current-project-root)])
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
                     [(null? preprocs-and-static-pagetrees)
                      (displayln (format "rendering generated pagetree for directory ~a" dir))
                      (cdr (make-project-pagetree dir))]
                     [else
                      (displayln (format "rendering preproc & pagetree files in directory ~a" dir))
                      preprocs-and-static-pagetrees])))
            (apply render-batch batch-to-render)))
        (begin ; first arg is a file
          (displayln (format "rendering ~a" (string-join (map ->string path-args) " ")))
          (apply render-batch path-args)))))

(define (handle-start directory [port #f])
  (when (not (directory-exists? directory))
    (error (format "~a is not a directory" directory)))
  (parameterize ([world:current-project-root directory]
                 [world:current-server-port (or port world:default-port)])
    (displayln "starting project server ...")
    ((dynamic-require 'pollen/server 'start-server))))


(define (handle-publish directory rest-args arg-command-name)
  (define target-path
    (or 
     (and rest-args (not (null? rest-args)) (path->complete-path (string->path (car rest-args))))
     (build-path (find-system-path 'desk-dir) (string->path (if (equal? arg-command-name "clone") "clone" (world:current-publish-directory-name))))))
  
  (define (delete-it path)
    (cond
      [(directory-exists? path) (delete-directory/files path)]
      [(file-exists? path) (delete-file path)]))
  
  (define (contains-directory? possible-superdir possible-subdir)
    (define (has-prefix? xs prefix)
      (and (>= (length xs) (length prefix))
           (andmap equal? prefix (take xs (length prefix)))))
    ((explode-path possible-subdir) . has-prefix? . (explode-path possible-superdir)))
  
  (define source-dir (simplify-path directory))
  (when (not (directory-exists? source-dir))
    (error 'publish (format "source directory ~a does not exist" source-dir)))
  (define target-dir (simplify-path target-path))
  (when (source-dir . contains-directory? . target-dir)
    (error 'publish "aborted because target directory for publishing (~a) can't be inside source directory (~a)" target-dir source-dir))
  (when (target-dir . contains-directory? . source-dir)
    (error 'publish "aborted because target directory for publishing (~a) can't contain source directory (~a)" target-dir source-dir))
  (when (equal? target-dir (current-directory))
    (error 'publish "aborted because target directory for publishing (~a) can't be the same as current directory (~a)" target-dir (current-directory)))
  (displayln "publishing ...")
  (when (directory-exists? target-dir)
    (delete-directory/files target-dir))
  (copy-directory/files source-dir target-dir)
  (parameterize ([world:current-project-root (current-directory)])
    (for-each delete-it (find-files pollen-related-file? target-dir)))
  (displayln (format "completed to ~a" target-dir)))

(define (handle-unknown command)
  (if (regexp-match #rx"(shit|fuck)" command)
      (displayln (let ([responses '("Cursing at free software? Really?" "How uncouth." "Same to you, buddy.")])
                   (list-ref responses (random (length responses)))))
      (displayln (format "unknown command ~a" command))))
