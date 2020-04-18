#lang debug racket/gui
(require pollen/private/log
         pollen/private/command)

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
                    [height 700]
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

(define (set-current-manager-directory val)
  (current-manager-directory val)
  (send directory-msg set-label (if val (path->string val) "")))

(define button-directory
  (let ([str "Select project directory"])
    (new button%	 
         [label str]	 
         [parent hpanel-select]
         [callback (λ (button evt) (set-current-manager-directory (get-directory str)))])))

(define directory-msg
  (new message%	 
       [label ""]	 
       [parent hpanel-select]
       [auto-resize #true]))

(define hpanel-server-controls (make-hpanel))

(define (make-alert str)
  (define d (new dialog%
                 [label str]
                 [width 300]
                 [height 300]
                 [parent #f]))
  #R d
  #R (send d show #f))

(define button-start
  (new button%	 
       [label "Start project server"]	 
       [parent hpanel-server-controls]
       [callback (λ (button evt)
                   (match (current-manager-directory)
                     [#false (make-alert "boo")]
                     [dir
                   (with-logging-to-port
                       (current-error-port)
                     (λ () (start-project-server dir))
                     #:logger pollen-logger
                     'info
                     'pollen)]))]))

(define button-stop
  (new button%	 
       [label "Stop project server"]	 
       [parent hpanel-server-controls]
       [callback (λ (button evt) (void))]))

(define wordbox (let* ([wb (new text-field%
                                [label #f]
                                [style '(multiple)]
                                [parent window]
                                [font (make-font #:face app-mono-fam #:size app-font-size)])]
                       [ed (send wb get-editor)])
                  (send ed set-line-spacing (* app-font-size 0.4))
                  (send ed set-padding 6 3 6 3)
                  wb))



(send window show #t)
