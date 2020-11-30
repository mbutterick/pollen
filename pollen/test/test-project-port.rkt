#lang racket/base

(require rackunit
         racket/port
         racket/runtime-path)

(define-runtime-path project-port-dir "data/project-port")

(define thd #f)
(define-values (in out) (make-pipe))
(parameterize ([exit-handler (lambda (code)
                               (fail (format "abnormal exit from raco command~n  code: ~a" code))
                               (kill-thread thd))])
  (set! thd
        (parameterize ([current-output-port out]
                       [current-error-port out]
                       [current-directory project-port-dir]
                       [current-command-line-arguments (vector "start")])
          (thread
           (lambda ()
             (dynamic-require '(submod pollen/private/command raco) #f)))))

  (dynamic-wind
    void
    (lambda ()
      (sync
       (handle-evt
        (regexp-match-evt #rx"project server is http://localhost:9876" in)
        void)
       (handle-evt
        (alarm-evt (+ (current-inexact-milliseconds) 5000))
        (lambda (_)
          (fail "timed out while waiting for server to start")))))
    (lambda ()
      (break-thread thd)
      (thread-wait thd))))
