#lang racket/base

(require racket/port
         racket/runtime-path
         racket/tcp
         rackunit)

(define-runtime-path project-port-dir "data/project-port")

(define the-port
  (dynamic-require
   `(submod ,(build-path project-port-dir "pollen.rkt") setup)
   'project-server-port))

(define-values (in out)
  (make-pipe))

(define thd
  (parameterize ([current-output-port out]
                 [current-error-port out]
                 [current-directory project-port-dir]
                 [current-command-line-arguments (vector "start")]
                 [exit-handler (lambda (code)
                                 (fail (format "abnormal exit from raco command~n  code: ~a" code))
                                 (kill-thread thd))])
    (thread
     (lambda ()
       (dynamic-require '(submod pollen/private/command raco) #f)))))

(dynamic-wind
  void
  (lambda ()
    (sync
     (handle-evt
      (regexp-match-evt #rx"ready to rock" in)
      void)
     (handle-evt
      (alarm-evt (+ (current-inexact-milliseconds) 5000))
      (lambda (_)
        (fail "timed out while waiting for server to start"))))

    (with-handlers ([exn:fail?
                     (lambda (e)
                       (fail (format "failed to connect to server: ~a" (exn-message e))))])
      (define-values (cin cout)
        (tcp-connect "127.0.0.1" the-port))
      (close-output-port cout)
      (close-input-port cin)))
  (lambda ()
    (break-thread thd)
    (thread-wait thd)))
