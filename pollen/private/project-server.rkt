#lang racket/base
(require racket/async-channel
         racket/runtime-path
         web-server/dispatch
         web-server/web-server
         web-server/servlet-dispatch
         web-server/private/mime-types
         (prefix-in files: web-server/dispatchers/dispatch-files)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         web-server/dispatchers/filesystem-map
         net/sendurl
         "project-server-routes.rkt" 
         "log.rkt" 
         "../setup.rkt"
         "../file.rkt"
         "version.rkt")

(provide start-server)

(define-runtime-path mime-types "server-extras/mime.types")

(define (make-static-dispatcher-sequence . pths)
  (apply sequencer:make
         (for/list ([pth (in-list pths)])
           (files:make
            #:path->mime-type (make-path->mime-type mime-types)
            #:url->path (make-url->path (path->string pth))))))

(define-values (pollen-servlet _)
  (dispatch-rules
   ;; last element of a "/"-terminated url is ""
   [((string-arg) ... "") route-index] 
   [((string-arg) ... (? pagetree-source?)) route-dashboard]
   [((string-arg) ... "in" (string-arg) ...) route-in]
   [((string-arg) ... "out" (string-arg) ...) route-out]
   [else route-default]))

(define (start-server servlet-path [open-browser-window? #false] #:return [return? #false])
  
  (define server-name (format "http://localhost:~a" (current-server-port)))

  (message (format "welcome to Pollen ~a (Racket ~a)" pollen:version (version)))
  (message (format "project root is ~a" (current-project-root)))
  (message (format "project server is ~a (Ctrl+C to exit)" server-name))
  (message (format "project dashboard is ~a/~a" server-name (setup:main-pagetree)))
  (message (let ([clsi (current-server-listen-ip)])
             (if clsi
                 (format "project server permitting access only to ~a"
                         (case clsi
                           [("127.0.0.1") "localhost"]
                           [else clsi]))
                 "project server permitting access to all clients")))
  (define ch (make-async-channel))
  (define stop-func
    (parameterize ([error-print-width 1000])
      (serve
       #:confirmation-channel ch
       #:dispatch (sequencer:make
                   (dispatch/servlet pollen-servlet)
                   (make-static-dispatcher-sequence
                    (current-project-root)
                    (current-server-extras-path))
                   (dispatch/servlet route-404))
       #:listen-ip (current-server-listen-ip)
       #:port (current-server-port))))
  (define exn-or-port
    (sync ch))
  (when (exn? exn-or-port)
    (message "project server failed to start")
    (sync (system-idle-evt))
    (exit 1))
  (message "ready to rock")
  (when open-browser-window?
    (send-url (string-append server-name servlet-path)))
  (if return?
      stop-func
      (with-handlers ([exn:break? (λ (e) (stop-func) (message "project server stopped"))])
        (do-not-return))))
