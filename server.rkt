#lang web-server
(require web-server/servlet-env 
         web-server/dispatch)
(require "server-routes.rkt" 
         "debug.rkt" 
         "world.rkt"
         "file-tools.rkt")

(define-values (pollen-servlet _)
  (dispatch-rules
   [((string-arg) ... (? (λ(x) (x . has-ext? . PTREE_SOURCE_EXT)))) route-dashboard]
   [((string-arg) ... "in" (string-arg)) route-in]
   [((string-arg) ... "out" (string-arg)) route-out]
   [((string-arg) ... "xexpr" (string-arg)) route-xexpr]
   [else route-default]))

(message (format "Welcome to Pollen ~a" POLLEN_VERSION) (format "(Racket ~a)" (version)))
(message (format "Project root is ~a" PROJECT_ROOT))

(define server-name (format "http://localhost:~a" SERVER_PORT))
(message (format "Project server is ~a" server-name) "(Ctrl-C to exit)")
(message (format "Project dashboard is ~a/~a" server-name DASHBOARD_NAME))

(message "Ready to rock")

(serve/servlet pollen-servlet
               #:port SERVER_PORT
               #:listen-ip #f
               #:servlet-regexp #rx"" ; respond to top level
               #:command-line? #t
               #:file-not-found-responder route-404
               #:extra-files-paths (list SERVER_EXTRAS_DIR PROJECT_ROOT))