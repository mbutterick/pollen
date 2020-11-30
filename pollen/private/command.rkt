#lang racket/base
(require racket/file
         racket/path
         racket/vector
         racket/cmdline
         racket/match
         sugar/coerce
         "file-utils.rkt"
         "log.rkt"
         "../setup.rkt"
         "../pagetree.rkt")

;; The use of dynamic-require throughout this file is intentional:
;; this way, low-dependency raco commands (like "version") are faster.
;; Whereas with `require` or `local-require`, everything would have to be front-loaded.
;; but ... maybe most of the latency is due to pollen/setup environment checking.
;; todo: investigate this

(module+ raco
  (define command-name (with-handlers ([exn:fail? (λ (exn) #f)])
                         (vector-ref (current-command-line-arguments) 0)))
  (dispatch command-name))

(define (get-first-arg-or-current-dir [args (cdr (vector->list (current-command-line-arguments)))]) ; cdr to strip command name from front
  (normalize-path
   (with-handlers ([exn:fail? (λ (exn) (current-directory))])
     ;; incoming path argument is handled as described in docs for current-directory
     (very-nice-path (car args)))))

(define (dispatch command-name)
  (define dispatch-thunk
    (λ ()
      (case command-name
        [("test" "xyzzy") (handle-test)]
        [(#f "help") (handle-help)]
        [("start") (handle-start)] ; parses its own args
        ;; "second" arg is actually third in command line args, so use cddr not cdr
        [("render") (handle-render)] ; render parses its own args from current-command-line-arguments
        [("version") (handle-version)]
        [("reset") (handle-reset (get-first-arg-or-current-dir))]
        [("setup") (handle-setup)]
        [("clone" "publish") (handle-publish)]
        [else (handle-unknown command-name)])))
  (cond
    [(let ([str (getenv "PLTSTDERR")])
       (and str (regexp-match "@pollen" str))) (dispatch-thunk)]
    [else (with-logging-to-port
           (current-error-port)
           dispatch-thunk
           #:logger pollen-logger
           'info
           'pollen)]))

(define (very-nice-path x)
  (simple-form-path (cleanse-path (->path x))))

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
  (message "resetting cache ...")
  ((dynamic-require 'pollen/cache 'reset-cache) directory-maybe))

(define (handle-setup)
  (message "preheating cache ...")
  (define setup-parallel? (make-parameter #false))
  (define dry-run? (make-parameter #false))
  (define parsed-args
    (command-line #:program "raco pollen setup"
                  #:argv (vector-drop (current-command-line-arguments) 1) ; snip the 'setup' from the front
                  #:once-any
                  [("-p" "--parallel") "Setup in parallel using all cores" (setup-parallel? #true)]
                  [("-j" "--jobs") job-count "Setup in parallel using <job-count> jobs" (setup-parallel? (or (string->number job-count) (raise-argument-error 'handle-setup "exact positive integer" job-count)))]
                  [("-d" "--dry-run") "Print paths that would be compiled" (dry-run? #true)]
                  #:args other-args
                  other-args))
  (define starting-dir (match parsed-args
                         [(list dir) dir]
                         [_  (current-directory)]))
  ((dynamic-require 'pollen/private/preheat-cache 'preheat-cache) starting-dir (setup-parallel?) (dry-run?)))

(define (handle-render)
  (define render-batch (dynamic-require 'pollen/render 'render-batch))
  (define string-join (dynamic-require 'racket/string 'string-join))
  (define make-project-pagetree (dynamic-require 'pollen/pagetree 'make-project-pagetree))
  (define render-target-wanted (make-parameter (current-poly-target)))
  (define render-with-subdirs? (make-parameter #f))
  (define render-parallel? (make-parameter #f))
  (define special-output? (make-parameter #f))
  (define force-render? (make-parameter #f))
  (define parsed-args
    (command-line #:program "raco pollen render"
                  #:argv (vector-drop (current-command-line-arguments) 1) ; snip the 'render' from the front
                  #:once-each
                  [("-t" "--target") target-arg "Render target for poly sources"
                                     (render-target-wanted (->symbol target-arg))]
                  [("-r" "--recursive") "Render subdirectories recursively"
                                        (render-with-subdirs? 'recursive)]
                  [("-s" "--subdir") "Render subdirectories nonrecursively" (render-with-subdirs? 'include)]
                  [("-f" "--force") "Force render" (force-render? #true)]
                  #:once-any
                  [("-d" "--dry-run") "Print paths that would be rendered" (special-output? 'dry-run)]
                  [("-n" "--null") "Suppress file output" (special-output? 'null)]
                  #:once-any
                  [("-p" "--parallel") "Render in parallel using all cores" (render-parallel? #true)]
                  [("-j" "--jobs") job-count "Render in parallel using <job-count> jobs" (render-parallel? (or (string->number job-count) (raise-argument-error 'handle-render "exact positive integer" job-count)))]
                  #:args other-args
                  other-args))

  (define timestamp (current-seconds)) ; keeps timestamp consistent through whole render
  (define (handle-batch-render paths)
    (when (force-render?)
      ;; forcing works like `touch`: updates the mod date of the files,
      ;; which invalidates any cached results.
      (let force-paths ([paths paths])
        (for* ([path (in-list paths)]
               [sp (in-value (if (pagetree-source? path) path (get-source path)))]
               #:when sp)
              (file-or-directory-modify-seconds sp timestamp)
              (when (pagetree-source? sp)
                (force-paths (pagetree->paths sp))))))
    (apply render-batch (map very-nice-path paths) #:parallel (render-parallel?) #:special (special-output?)))
  
  (parameterize ([current-poly-target (render-target-wanted)]) ;; applies to both cases
    (let loop ([args parsed-args])
      (match args
        [(? null?) (loop (list (current-directory)))]
        [(list dir) ;; directory mode: one directory as argument
         #:when (directory-exists? dir)
         (define top-dir (very-nice-path dir))
         (let render-one-dir ([dir top-dir])
           (when (or (extra-path? dir) (not (omitted-path? dir)))
             (parameterize ([current-directory dir]
                            [current-project-root (case (render-with-subdirs?)
                                                    [(recursive) dir]
                                                    [else top-dir])])
               (define dirlist (directory-list dir))
               (define paths-to-render
                 (match (filter pagetree-source? dirlist)
                   ;; if there are no static pagetrees, use make-project-pagetree
                   ;; (which will synthesize a pagetree if needed, which includes all sources)
                   [(? null?)
                    (message (format "rendering generated pagetree for directory ~a" dir))
                    (cdr (make-project-pagetree dir))]
                   [pagetree-sources
                    (message (format "rendering preproc & pagetree files in directory ~a" dir))
                    (append (filter preproc-source? dirlist) pagetree-sources)]))
               (handle-batch-render paths-to-render)
               (when (render-with-subdirs?)
                 (for ([path (in-list dirlist)]
                       #:when (directory-exists? path))
                      (render-one-dir (->complete-path path)))))))]
        [path-args ;; path mode
         (message (format "rendering ~a" (string-join (map ->string path-args) " ")))
         (handle-batch-render path-args)]))))

(define (handle-start)
  (define launch-wanted #f)
  (define localhost-wanted #f)
  (define-values (dir http-port)
    (command-line
     #:program "raco pollen start"
     #:argv (vector-drop (current-command-line-arguments) 1) ; snip the 'start' from the front
     #:once-each
     [("--launch" "-l") "Launch browser after start" (set! launch-wanted #t)]
     [("--local") "Restrict access to localhost" (set! localhost-wanted #t)]
     #:args ([dir (current-directory)] [port #f])
     (define parsed-dir
       (path->directory-path (normalize-path (very-nice-path dir))))
     (unless (directory-exists? parsed-dir)
       (error (format "~a is not a directory" parsed-dir)))

     (define parsed-port (and port (string->number port)))
     (when (and parsed-port (not (exact-positive-integer? parsed-port)))
       (error (format "~a is not a valid port number" parsed-port)))
     (values parsed-dir parsed-port)))
  (parameterize ([current-project-root dir]
                 [current-server-port (or http-port (setup:project-server-port))]
                 [current-server-listen-ip (and localhost-wanted "127.0.0.1")]
                 [current-session-interactive? #true])
    (message "starting project server ...")
    ((dynamic-require 'pollen/private/project-server 'start-server) (format "/~a" (setup:main-pagetree dir)) launch-wanted)))

(define (make-publish-dir-name [project-root (current-directory)] [arg-command-name #f])
  (define user-publish-path
    (expand-user-path (->path (setup:publish-directory project-root))))
  (if (complete-path? user-publish-path)
      user-publish-path       
      (build-path (find-system-path 'desk-dir)
                  (->path (case arg-command-name
                            [("clone") "clone"] ; bw compat
                            [else user-publish-path])))))

(define (delete-it! path)
  (match path
    [(? directory-exists?) (delete-directory/files path)]
    [(? file-exists?) (delete-file path)]
    ;; possible we'll get a file path whose parent directory
    ;; has been deleted (and so it too is already gone)
    [_ (void)]))

(define (contains-directory? possible-superdir possible-subdir)
  (define (has-prefix? xs prefix)
    (and (>= (length xs) (length prefix))
         (andmap equal? prefix (for/list ([(x idx) (in-indexed xs)]
                                          #:break (= idx (length prefix)))
                                         x))))
  ((explode-path possible-subdir) . has-prefix? . (explode-path possible-superdir)))

(define (handle-publish)
  (define command-name ; either "publish" or "clone"
    (vector-ref (current-command-line-arguments) 0))
  (define force-target-overwrite? (make-parameter #true))
  (define dry-run? (make-parameter #false))
  (define other-args (command-line
                      ;; drop command name
                      #:argv (vector-drop (current-command-line-arguments) 1)
                      #:once-each
                      [("-c" "--confirm") "Confirm overwrite of existing dest dir"
                                          (force-target-overwrite? #f)]
                      [("-d" "--dry-run") "Check paths that would be published" (dry-run? #true)]
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
  
  (define do-publish-operation?
    (or (not (directory-exists? dest-dir))
        (force-target-overwrite?)
        (begin
          (display (format "destination directory ~a exists. Overwrite? [yes/no] " dest-dir))
          (case (read)
            [(y yes) #true]
            [else #false]))))
  (cond
    [(dry-run?)
     (message "publish: start dry run")
     (message (format "would publish from ~a to ~a" source-dir dest-dir))
     (cond
       [(directory-exists? dest-dir)
        (message (string-append (format "directory ~a exists (but can be overwritten)" dest-dir)))]
       [(directory-exists? (simplify-path (build-path dest-dir "..")))
        (message (string-append (format "directory ~a does not exist (but can be created)" dest-dir)))]
       [else
        (raise-user-error 'publish "dry run failure: directory path ~a is defective (neither directory nor parent directory exists)" dest-dir)])
     (message "publish: end dry run")]
    [else
     (message (string-append (format "publishing from ~a to ~a ..." source-dir dest-dir)))
     (cond
       [do-publish-operation?
        (when (directory-exists? dest-dir)
          (with-handlers ([exn:fail:filesystem? (λ (exn) (raise-user-error 'publish (format "operation failed: could not delete ~a" dest-dir)))])
            (delete-directory/files dest-dir)))
        (copy-directory/files source-dir dest-dir)
        ;; if source-dir is provided, we want it to be treated as current-directory.
        ;; if no source-dir is provided, it is set to current-directory,
        ;; so the parameterize is a no-op.
        (parameterize* ([current-directory source-dir]
                        [current-project-root (current-directory)])
          (define (delete-from-publish-dir? p)
            (and (omitted-path? p) (not (extra-path? p))))
          (for-each delete-it! (find-files delete-from-publish-dir? dest-dir)))
        (message "publish completed")]
       [else (message "publish aborted")])]))

(define (handle-unknown command)
  (match command
    [(regexp #rx"(shit|fuck)")
     (define responses '("Cursing at free software? Really?" "How uncouth." "Same to you, buddy."))
     (displayln (list-ref responses (random (length responses))))]
    [_ (displayln (format "`~a` is an unknown command." command))
       (display "These are the available ") ; ... "Pollen commands:"
       (handle-help)
       (exit 1)]))
