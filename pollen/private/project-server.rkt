#lang web-server/base
(require racket/runtime-path
         web-server/servlet-env 
         web-server/dispatch
         "project-server-routes.rkt" 
         "log.rkt" 
         "../setup.rkt"
         "../file.rkt"
         "version.rkt")

(provide start-server)

(define-runtime-path mime-types "server-extras/mime.types")

(define (start-server servlet-path [open-browser-window? #f]
                      #:serve-only [serve-only? #false])
  (define-values (pollen-servlet _)
    (dispatch-rules
     [((string-arg) ... (? (λ (x) (string=? "" x)))) route-index] ; last element of a "/"-terminated url is ""
     [((string-arg) ... (? pagetree-source?)) route-dashboard]
     [((string-arg) ... "in" (string-arg) ...) route-in]
     [((string-arg) ... "out" (string-arg) ...) route-out]
     [else route-default]))
  
  (message (format "welcome to Pollen ~a (Racket ~a)" pollen:version (version)))
  (message (format "project root is ~a" (current-project-root)))
  
  (define server-name (format "http://localhost:~a" (current-server-port)))
  (message (format "project server is ~a (Ctrl+C to exit)" server-name))
  (message (format "project dashboard is ~a/~a" server-name (setup:main-pagetree)))
  (message (if (current-server-listen-ip)
               (format "project server permitting access only to ~a"
                       (case (current-server-listen-ip)
                         [("127.0.0.1") "localhost"]
                         [else (current-server-listen-ip)]))
               "project server permitting access to all clients"))
  (message "ready to rock")

  (cond
    [serve-only?
     (define stopper
       ((dynamic-require 'web-server/web-server 'serve)
        #:dispatch ((dynamic-require 'web-server/servlet-dispatch 'dispatch/servlet) pollen-servlet)
        #:listen-ip (current-server-listen-ip)
        #:port (current-server-port)))
     stopper]
    [else
     (parameterize ([error-print-width 1000])
       (serve/servlet pollen-servlet
                      #:launch-browser? open-browser-window?
                      #:servlet-path servlet-path
                      #:port (current-server-port)
                      #:listen-ip (current-server-listen-ip)
                      #:servlet-regexp #rx"" ; respond to top level
                      #:command-line? #true
                      #:file-not-found-responder route-404
                      #:mime-types-path mime-types
                      #:extra-files-paths (list (current-server-extras-path) (current-project-root))))]))