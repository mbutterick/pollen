#lang debug racket/gui
(require pollen/private/log
         pollen/private/command
         pollen/setup
         pollen/private/project-server)

(define app-mono-fam
  (for*/first ([preferred
                '("Triplicate T4" "Menlo" "Consolas" "Andale Mono" "Courier")]
               [mono-fam (in-list (get-face-list 'mono))]               
               #:when (equal? preferred mono-fam))
    preferred))

(define app-font-size 14)
(define app-font (make-font #:face (send normal-control-font get-face) #:size app-font-size))

(define window (new frame% [label "Pollen Helper"]
                    [width 700]
                    [height 500]
                    [x 40]
                    [y 40]
                    [alignment '(left top)]
                    [spacing 6]
                    [border 6]))

(define (make-hpanel) (new horizontal-panel%
                           [parent window]
                           [alignment '(left top)]
                           [stretchable-height #false]))

(define hpanel-select (make-hpanel))
(define current-manager-directory (make-parameter #f))
(define current-server-stopper (make-parameter #f))

(define (set-current-manager-directory val)
  (current-manager-directory val)
  (send directory-msg set-label (if val (path->string val) "")))

(module+ main
(set-current-manager-directory (expand-user-path "~/git/bpt/")))

(define button-directory
  (let ([str "Select project directory"])
    (new button%	 
         [label str]	 
         [parent hpanel-select]
         [callback (λ (button evt)
                     (define dir (get-directory str))
                     (when dir
                       (set-current-manager-directory dir)
                       (set-server-port (setup:project-server-port dir))))])))

(define directory-msg
  (new message%	 
       [label ""]	 
       [parent hpanel-select]
       [auto-resize #true]))

(define hpanel-server-options (make-hpanel))

(define server-option-port
  (new text-field%
       [parent hpanel-server-options]
       [label "server port"]
       [min-width 150]
       [stretchable-width #false]
       [init-value (number->string (setup:project-server-port))]
       [callback (λ (tf evt)
                   (set-server-port (string->number (send tf get-value))))]))

(define (set-server-port num)
  (when (number? num)
    (current-server-port num)
    (send server-option-port set-value (number->string num))))
  

(define server-option-local
  (new check-box%
       [parent hpanel-server-options]
       [label "local only"]
       [callback (λ (cb evt)
                   (current-server-listen-ip (and (send cb get-value) "127.0.0.1")))]))

(define hpanel-server-controls (make-hpanel))

(define dialog-alert (new dialog%
                          [label "Error"]
                          [border 6]
                          [spacing 6]
                          [parent #f]))

(define dialog-message (new message%
                            [parent dialog-alert]
                            [auto-resize #t]
                            [label ""]))

(define dialog-alert-button-pane
  (new horizontal-pane%
       [parent dialog-alert]
       [alignment '(right bottom)]))

(define dialog-alert-button-ok
  (new button%
       [parent dialog-alert-button-pane]
       [label "OK"]
       [style '(border)]
       [callback (λ (button evt)
                   (send dialog-alert show #f))]))

(define (make-alert str)
  (send dialog-message set-label str)
  (send dialog-alert show #t))


(define button-start
  (new button%	 
       [label "Start project server"]	 
       [parent hpanel-server-controls]
       [callback (λ (button evt)
                   (match (current-manager-directory)
                     [#false (make-alert "No project directory selected.")]
                     [dir 
                      (message "starting project server")
                      (define cthd (current-thread))
                      (define server-thd
                        (thread (λ ()
                                  (parameterize ([current-project-root dir])
                                    (define stop
                                      (start-server (setup:main-pagetree dir) #:serve-only #true))
                                    (thread-send cthd stop)))))
                      (current-server-stopper (thread-receive))]))]))

(define button-stop
  (new button%	 
       [label "Stop project server"]	 
       [parent hpanel-server-controls]
       [callback (λ (button evt)
                   (match (current-server-stopper)
                     [#false (make-alert "Project server not running.")]
                     [stopper-proc 
                      (stopper-proc)
                      (current-server-stopper #false)
                      (message "project server stopped")]))]))

(define button-launch
  (new button%	 
       [label "Launch browser"]	 
       [parent hpanel-server-controls]
       [callback (λ (button evt)
                   (match (current-server-stopper)
                     [#false (make-alert "Project server not running.")]
                     [dir #R 'wish-i-could-launch]))]))

(define status-box
  (let* ([wb (new text-field%
                  [label #f]
                  [style '(multiple)]
                  [parent window]
                  [font (make-font #:face app-mono-fam #:size app-font-size)])]
         [ed (send wb get-editor)])
    (send ed set-line-spacing (* app-font-size 0.4))
    (send ed set-padding 6 3 6 3)
    wb))

(define status-box-ed (send status-box get-editor))

(define log-receiver-thread
  (thread (λ ()
            (define rcvr (make-log-receiver pollen-logger 'info 'pollen))
            (for ([vec (in-producer (λ () (sync rcvr)))])
              (match-define (vector _ msg _ _) vec)
              (send status-box-ed insert msg)
              (send status-box-ed insert "\n")
              (sleep 0)))))


(send window show #t)
