#lang racket/base
(require racket/file
         racket/path
         racket/string
         racket/list
         racket/vector
         racket/cmdline
         sugar/coerce
         "file-utils.rkt"
         "../setup.rkt"
         "../render.rkt"
         "../pagetree.rkt")

;; The use of dynamic-require throughout this file is intentional:
;; this way, low-dependency raco commands (like "version") are faster.
;; Whereas with `require` or `local-require`, everything would have to be front-loaded.
;; but ... maybe most of the latency is due to pollen/setup environment checking.
;; todo: investigate this

(module+ raco
  (define command-name (with-handlers ([exn:fail? (λ _ #f)])
                         (vector-ref (current-command-line-arguments) 0)))
  (dispatch command-name))


(define (get-first-arg-or-current-dir [args (cdr (vector->list (current-command-line-arguments)))]) ; cdr to strip command name from front
  (normalize-path
   (with-handlers ([exn:fail? (λ (exn) (current-directory))])
     ;; incoming path argument is handled as described in docs for current-directory
     (very-nice-path (car args)))))


(define (dispatch command-name)
  (case command-name
    [("test" "xyzzy") (handle-test)]
    [(#f "help") (handle-help)]
    [("start") (handle-start)] ; parses its own args
    ;; "second" arg is actually third in command line args, so use cddr not cdr
    [("render") (handle-render)] ; render parses its own args from current-command-line-arguments
    [("version") (handle-version)]
    [("reset") (handle-reset (get-first-arg-or-current-dir))]
    [("setup") (handle-setup (get-first-arg-or-current-dir))]
    [("clone" "publish") (handle-publish)]
    [else (handle-unknown command-name)]))

(define (very-nice-path x)
  (path->complete-path (simplify-path (cleanse-path (->path x)))))

(define (handle-test)
  (displayln "raco pollen is installed correctly"))

(define (handle-help)
  (displayln (format "Pollen commands:
help                   show this message
start [dir] [port]     starts project server in dir (default is current dir) 
                          (default port is ~a)
render [dir]           render project in dir (default is current dir) 
render path ...        render one or more paths (can be source or output name)
publish                copy project to ~a without source files
publish [dir] [dest]   copy project in dir to dest without source files
                          (warning: overwrites existing dest)
setup                  preload cache
reset                  reset cache
version                print the version" (current-server-port) (make-publish-dir-name))))


(define (handle-version)
  (displayln (dynamic-require 'pollen/private/version 'pollen:version)))


(define (handle-reset directory-maybe)
  (displayln "resetting cache ...")
  ((dynamic-require 'pollen/cache 'reset-cache) directory-maybe))


(define (handle-setup directory-maybe)
  (displayln "preheating cache ...")
  ((dynamic-require 'pollen/private/preheat-cache 'preheat-cache) directory-maybe)) 


(define (handle-render)
  (define render-target-wanted (make-parameter (current-poly-target)))
  (define render-with-subdirs? (make-parameter #f))
  (define parsed-args (command-line #:program "raco pollen render"
                                    #:argv (vector-drop (current-command-line-arguments) 1) ; snip the 'render' from the front
                                    #:once-each
                                    [("-t" "--target") target-arg "Render target for poly sources"
                                                       (render-target-wanted (->symbol target-arg))]
                                    [("-r" "--recursive") "Render subdirectories recursively"
                                                          (render-with-subdirs? 'recursive)]
                                    [("-s" "--subdir") "Render subdirectories nonrecursively" (render-with-subdirs? 'include)]
                                    #:args other-args
                                    other-args))  
  (define path-args (if (empty? parsed-args)
                        (list (current-directory))
                        parsed-args))
  (parameterize ([current-directory (current-project-root)]
                 [current-poly-target (render-target-wanted)])
    ;; special case: one directory as argument
    (if (and (= 1 (length path-args)) (directory-exists? (car path-args)))
        (let render-one-dir ([dir (->complete-path (car path-args))])
          (parameterize ([current-directory dir]
                         [current-project-root (case (render-with-subdirs?)
                                                 [(recursive) dir]
                                                 [else (current-project-root)])])
            (define dirlist (directory-list dir))
            (define preprocs (filter preproc-source? dirlist))
            (define static-pagetrees (filter pagetree-source? dirlist))
            ;; if there are no static pagetrees, use make-project-pagetree
            ;; (which will synthesize a pagetree if needed, which includes all sources)
            (define batch-to-render
              (map very-nice-path
                   (cond
                     [(null? static-pagetrees)
                      (displayln (format "rendering generated pagetree for directory ~a" dir))
                      (cdr (make-project-pagetree dir))]
                     [else
                      (displayln (format "rendering preproc & pagetree files in directory ~a" dir))
                      (append preprocs static-pagetrees)])))
            (apply render-batch batch-to-render)
            (when (render-with-subdirs?)
              (for ([path (in-list dirlist)]
                    #:when (and (directory-exists? path)
                                (not (omitted-path? path))))
                   (render-one-dir (->complete-path path))))))
        (begin
          (displayln (format "rendering ~a" (string-join (map ->string path-args) " ")))
          (apply render-batch path-args)))))


(define (handle-start)
  (define launch-wanted #f)
  (define localhost-wanted #f)
  (define clargs (command-line #:program "raco pollen start"
                               #:argv (vector-drop (current-command-line-arguments) 1) ; snip the 'start' from the front
                               #:once-each
                               [("--launch" "-l") "Launch browser after start" (set! launch-wanted #t)]
                               [("--local") "Restrict access to localhost" (set! localhost-wanted #t)]
                               #:args other-args
                               other-args))
  (define dir (path->directory-path (get-first-arg-or-current-dir clargs)))
  (unless (directory-exists? dir)
    (error (format "~a is not a directory" dir)))
  (define port (with-handlers ([exn:fail? (λ (e) #f)])
                 (string->number (cadr clargs))))
  (when (and port (not (exact-positive-integer? port)))
    (error (format "~a is not a valid port number" port)))
  (parameterize ([current-project-root dir]
                 [current-server-port (or port (setup:project-server-port))]
                 [current-server-listen-ip (and localhost-wanted "127.0.0.1")])
    (displayln "Starting project server ...")
    ((dynamic-require 'pollen/private/project-server 'start-server) (format "/~a" (setup:main-pagetree dir)) launch-wanted)))


(define (make-publish-dir-name [project-root (current-directory)] [arg-command-name #f])
  (define user-publish-path
    (expand-user-path (->path (setup:publish-directory project-root))))
  (if (complete-path? user-publish-path)
      user-publish-path       
      (build-path (find-system-path 'desk-dir)
                  (->path (if (equal? arg-command-name "clone") ; bw compat
                              "clone"
                              user-publish-path)))))


(define (delete-it path)
  (cond
    [(directory-exists? path) (delete-directory/files path)]
    [(file-exists? path) (delete-file path)]))
  

(define (contains-directory? possible-superdir possible-subdir)
  (define (has-prefix? xs prefix)
    (and (>= (length xs) (length prefix))
         (andmap equal? prefix (take xs (length prefix)))))
  ((explode-path possible-subdir) . has-prefix? . (explode-path possible-superdir)))


(define (handle-publish)
  (define command-name ; either "publish" or "clone"
    (vector-ref (current-command-line-arguments) 0))
  (define force-target-overwrite? (make-parameter #t))
  (define other-args (command-line
                      ;; drop command name
                      #:argv (vector-drop (current-command-line-arguments) 1)
                      #:once-each
                      [("-c" "--confirm") "Confirm overwrite of existing dest dir"
                                          (force-target-overwrite? #f)]
                      #:args other-args
                      other-args))
  ;; other-args looks like (list [maybe-source-dir-arg] [maybe-dest-dir-arg])
  (define source-dir (simplify-path (get-first-arg-or-current-dir other-args)))
  (define dest-dir
    (simplify-path
     ;; the source-dir might have its own pollen.rkt specifying a publish destination
     (with-handlers ([exn:fail? (λ (exn) (make-publish-dir-name source-dir command-name))]) 
       (path->complete-path (string->path (cadr other-args))))))
  
  (unless (directory-exists? source-dir)
    (error 'publish (format "source directory ~a does not exist" source-dir)))
  
  (when (source-dir . contains-directory? . dest-dir)
    (error 'publish "aborted because destination directory for publishing (~a) can't be inside source directory (~a)" dest-dir source-dir))
  
  (when (dest-dir . contains-directory? . source-dir)
    (error 'publish "aborted because destination directory for publishing (~a) can't contain source directory (~a)" dest-dir source-dir))
  
  (when (equal? dest-dir (current-directory))
    (error 'publish "aborted because destination directory for publishing (~a) can't be the same as current directory (~a)" dest-dir (current-directory)))
  
  (display (format "publishing from ~a " source-dir))
  (displayln (format "to ~a ..." dest-dir))
  (define do-publish-operation?
    (or (not (directory-exists? dest-dir))
        (force-target-overwrite?)
        (begin
          (display (format "destination directory ~a exists. Overwrite? [yes/no] " dest-dir))
          (case (read)
            [(y yes) #t]
            [else #f]))))
  (cond
    [do-publish-operation?
     (when (directory-exists? dest-dir)
       (delete-directory/files dest-dir))
     (copy-directory/files source-dir dest-dir)
     ;; if source-dir is provided, we want it to be treated as current-directory.
     ;; if no source-dir is provided, it is set to current-directory,
     ;; so the parameterize is a no-op.
     (parameterize* ([current-directory source-dir]
                     [current-project-root (current-directory)])
       (define (delete-from-publish-dir? p)
         (and (omitted-path? p) (not (extra-path? p))))
       (for-each delete-it (find-files delete-from-publish-dir? dest-dir)))
     (displayln "publish completed")]
    [else (displayln "publish aborted")]))

(define (handle-unknown command)
  (if (regexp-match #rx"(shit|fuck)" command)
      (displayln (let ([responses '("Cursing at free software? Really?" "How uncouth." "Same to you, buddy.")])
                   (list-ref responses (random (length responses)))))
      (begin
        (displayln (format "`~a` is an unknown command." command))
        (display "These are the available ") ; ... "Pollen commands:"
        (handle-help))))
