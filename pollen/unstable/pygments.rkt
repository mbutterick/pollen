#lang rackjure/base

(require racket/function
         racket/match
         racket/port
         racket/runtime-path
         racket/system
         racket/string
         racket/format
         rackjure/threading
         xml
         (only-in html read-html-as-xml)
         "../private/log.rkt"
         "../private/splice.rkt")

(provide highlight make-highlight-css)


#|
A small mashup of Greg Hendershott's pygments.rkt, html.rkt, and params.rkt,
part of Frog, the static-blog generator
http://github.com/greghendershott/frog

YOU MUST HAVE PYGMENTS INSTALLED ALREADY!
sudo easy_install --upgrade Pygments

Sample usage:

#lang pollen/markup

◊(require pollen/unstable/pygments)
◊(make-highlight-css)

; specify language in square brackets
; put code in curly brackets
◊highlight['python]{ 
for x in range(10):
    print x
if zero is False:
    print "Hello world"}

|#

(define prn0 void)
(define prn1 message)

;;;;;;;;;;;;;;;;;;
;; from frog/html

(define (read-html-as-xexprs) ;; (-> (listof xexpr?))
  (~>> (read-html-as-xml)
       (element #f #f 'root '())
       xml->xexpr
       decode-ampersands-in-attributes
       cddr))

(define (decode-ampersands-in-attributes x)
  (match x
    [`(,tag ([,ks ,vs] ...) . ,els)
     `(,tag
       ,(for/list ([k ks]
                   [v vs])
          (list k (regexp-replace* "&amp;" v "\\&")))
       ,@(map decode-ampersands-in-attributes els))]
    [v v]))

;; end frog/html
;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;
;; from frog/pygments

;; Process that runs Python with our pipe.py script.

(define-values (pyg-in pyg-out pyg-pid pyg-err pyg-proc)
  (values #f #f #f #f #f))
(define-runtime-path pipe.py "../private/external/pipe.py")

(define start
  (let ([start-attempted? #f])
    (λ (python-executable line-numbers? css-class)
      (unless start-attempted?
        (set! start-attempted? #t)
        (define pre " Using plain `pre` blocks.")
        (match (find-executable-path python-executable)
          [(? path? python)
           (prn1 (~a "launching pipe.py to use Pygments"))
           (match (process (~a python
                               " -u " pipe.py
                               (if line-numbers? " --linenos" "")
                               " --cssclass " css-class))
             [(list in out pid err proc)
              (set!-values (pyg-in pyg-out pyg-pid pyg-err pyg-proc)
                           (values in out pid err proc))
              (file-stream-buffer-mode out 'line)
              (match (read-line pyg-in 'any)  ;; consume "ready" line or EOF
                [(? eof-object?) (prn0 (~a "Pygments pipe.py not responding" pre))]
                [_ (void)])]
             [_ (prn0 (~a "`" python " " pipe.py "` failed." pre))])]
          [#f
           (prn0 (~a "Pygments executable `" python-executable "` not found" pre))])))))

(define (running?)
  (and pyg-proc
       (eq? (pyg-proc 'status) 'running)))

(define (stop) ;; -> void
  (when (running?)
    (displayln "__EXIT__" pyg-out)
    (begin0 (or (pyg-proc 'exit-code) (pyg-proc 'kill))
      (close-input-port pyg-in)
      (close-output-port pyg-out)
      (close-input-port pyg-err)))
  (void))

(exit-handler
 (let ([old-exit-handler (exit-handler)])
   (lambda (v)
     (stop)
     (old-exit-handler v))))

(define (pygmentize code lang
                    #:python-executable python-executable
                    #:line-numbers? line-numbers?
                    #:css-class css-class
                    #:hl-lines hl-lines)
  (define (default code)
    `((pre () (code () ,code))))
  (unless (running?)
    (start python-executable line-numbers? css-class))
  (cond [(running?)
         ;; This works with a simple wrapper around Pygments defined in pipe.py.
         ;; First send over the configuration options, then code to highlight.
         ;; pipe.py also supports an encoding option, but seems unnecessary to
         ;; use at this point.
         (fprintf pyg-out "__LANG__ ~a~n" lang)
         (fprintf pyg-out "__LINES__ ~a~n" (string-join (map number->string hl-lines) " "))
         (fprintf pyg-out "__LINENOS__ ~a~n"
                  (if line-numbers? "true" "false"))
         (displayln code pyg-out)
         (displayln "__END__" pyg-out)

         ;; Read back the highlighted code
         (let loop ([s ""])
           (match (read-line pyg-in 'any)
             ["__END__" (with-input-from-string s read-html-as-xexprs)]
             [(? string? v) (loop (~a s v "\n"))]
             [_ (copy-port pyg-err (current-output-port)) ;echo error msg
                (default code)]))]
        [else (default code)]))

;; end frog/pygments
;;;;;;;;;;;;;;;;;;


(define (highlight #:python-executable [python-executable "python"]
                   #:line-numbers? [line-numbers? #t]
                   #:css-class [css-class "source"]
                   #:lines [hl-lines null]
                   lang . codelines)
  (define code (string-append* codelines))
  `(div ((class "highlight"))
        ,@(strip-empty-attrs (pygmentize code lang
                                         #:python-executable python-executable
                                         #:line-numbers? line-numbers?
                                         #:css-class css-class
                                         #:hl-lines hl-lines))))

;; Other CSS options available from http://richleland.github.io/pygments-css/ 

(define (make-highlight-css) '(style ((type "text/css")) 
".highlight .hll { background-color: #ffffcc }
.highlight  { background: #f8f8f8; }
.highlight .c { color: #408080; font-style: italic } /* Comment */
.highlight .err { border: 1px solid #FF0000 } /* Error */
.highlight .k { color: #008000; font-weight: bold } /* Keyword */
.highlight .o { color: #666666 } /* Operator */
.highlight .cm { color: #408080; font-style: italic } /* Comment.Multiline */
.highlight .cp { color: #BC7A00 } /* Comment.Preproc */
.highlight .c1 { color: #408080; font-style: italic } /* Comment.Single */
.highlight .cs { color: #408080; font-style: italic } /* Comment.Special */
.highlight .gd { color: #A00000 } /* Generic.Deleted */
.highlight .ge { font-style: italic } /* Generic.Emph */
.highlight .gr { color: #FF0000 } /* Generic.Error */
.highlight .gh { color: #000080; font-weight: bold } /* Generic.Heading */
.highlight .gi { color: #00A000 } /* Generic.Inserted */
.highlight .go { color: #888888 } /* Generic.Output */
.highlight .gp { color: #000080; font-weight: bold } /* Generic.Prompt */
.highlight .gs { font-weight: bold } /* Generic.Strong */
.highlight .gu { color: #800080; font-weight: bold } /* Generic.Subheading */
.highlight .gt { color: #0044DD } /* Generic.Traceback */
.highlight .kc { color: #008000; font-weight: bold } /* Keyword.Constant */
.highlight .kd { color: #008000; font-weight: bold } /* Keyword.Declaration */
.highlight .kn { color: #008000; font-weight: bold } /* Keyword.Namespace */
.highlight .kp { color: #008000 } /* Keyword.Pseudo */
.highlight .kr { color: #008000; font-weight: bold } /* Keyword.Reserved */
.highlight .kt { color: #B00040 } /* Keyword.Type */
.highlight .m { color: #666666 } /* Literal.Number */
.highlight .s { color: #BA2121 } /* Literal.String */
.highlight .na { color: #7D9029 } /* Name.Attribute */
.highlight .nb { color: #008000 } /* Name.Builtin */
.highlight .nc { color: #0000FF; font-weight: bold } /* Name.Class */
.highlight .no { color: #880000 } /* Name.Constant */
.highlight .nd { color: #AA22FF } /* Name.Decorator */
.highlight .ni { color: #999999; font-weight: bold } /* Name.Entity */
.highlight .ne { color: #D2413A; font-weight: bold } /* Name.Exception */
.highlight .nf { color: #0000FF } /* Name.Function */
.highlight .nl { color: #A0A000 } /* Name.Label */
.highlight .nn { color: #0000FF; font-weight: bold } /* Name.Namespace */
.highlight .nt { color: #008000; font-weight: bold } /* Name.Tag */
.highlight .nv { color: #19177C } /* Name.Variable */
.highlight .ow { color: #AA22FF; font-weight: bold } /* Operator.Word */
.highlight .w { color: #bbbbbb } /* Text.Whitespace */
.highlight .mf { color: #666666 } /* Literal.Number.Float */
.highlight .mh { color: #666666 } /* Literal.Number.Hex */
.highlight .mi { color: #666666 } /* Literal.Number.Integer */
.highlight .mo { color: #666666 } /* Literal.Number.Oct */
.highlight .sb { color: #BA2121 } /* Literal.String.Backtick */
.highlight .sc { color: #BA2121 } /* Literal.String.Char */
.highlight .sd { color: #BA2121; font-style: italic } /* Literal.String.Doc */
.highlight .s2 { color: #BA2121 } /* Literal.String.Double */
.highlight .se { color: #BB6622; font-weight: bold } /* Literal.String.Escape */
.highlight .sh { color: #BA2121 } /* Literal.String.Heredoc */
.highlight .si { color: #BB6688; font-weight: bold } /* Literal.String.Interpol */
.highlight .sx { color: #008000 } /* Literal.String.Other */
.highlight .sr { color: #BB6688 } /* Literal.String.Regex */
.highlight .s1 { color: #BA2121 } /* Literal.String.Single */
.highlight .ss { color: #19177C } /* Literal.String.Symbol */
.highlight .bp { color: #008000 } /* Name.Builtin.Pseudo */
.highlight .vc { color: #19177C } /* Name.Variable.Class */
.highlight .vg { color: #19177C } /* Name.Variable.Global */
.highlight .vi { color: #19177C } /* Name.Variable.Instance */
.highlight .il { color: #666666 } /* Literal.Number.Integer.Long */"))
