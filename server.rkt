#lang web-server
(require web-server/servlet-env 
         web-server/dispatch)
(require "server-routes.rkt" 
         "debug.rkt" 
         "world.rkt"
         "file-tools.rkt"
         "cache.rkt")

(define-values (pollen-servlet _)
  (dispatch-rules
   [((string-arg) ... (? (λ(x) (x . has-ext? . PTREE_SOURCE_EXT)))) route-dashboard]
   [((string-arg) ... "in" (string-arg)) route-in]
   [((string-arg) ... "out" (string-arg)) route-out]
   [((string-arg) ... "xexpr" (string-arg)) route-xexpr]
   [else route-default]))

(message (format "Welcome to Pollen ~a" POLLEN_VERSION) (format "(Racket ~a)" (version)))
(message (format "Project root is ~a" (CURRENT_PROJECT_ROOT)))

(define server-name (format "http://localhost:~a" SERVER_PORT))
(message (format "Project server is ~a" server-name) "(Ctrl-C to exit)")
(message (format "Project dashboard is ~a/~a" server-name DASHBOARD_NAME))

(message "Ready to rock")

(define MODULE_ROOT (apply build-path (drop-right (explode-path (current-contract-region)) 1)))
(define SERVER_EXTRAS_DIR (build-path MODULE_ROOT "pollen-server-extras"))

(parameterize ([current-cache (make-cache)])
  (serve/servlet pollen-servlet
                 #:port SERVER_PORT
                 #:listen-ip #f
                 #:servlet-regexp #rx"" ; respond to top level
                 #:command-line? #t
                 #:file-not-found-responder route-404
                 #:extra-files-paths (list SERVER_EXTRAS_DIR (CURRENT_PROJECT_ROOT))))