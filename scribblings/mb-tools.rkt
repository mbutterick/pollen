#lang racket/base
(require (for-syntax racket/base) scribble/core scribble/base scribble/manual racket/list scribble/private/manual-sprop scribble/decode scribble/html-properties racket/runtime-path racket/string)

(provide (all-defined-out))

(define-runtime-path mb-css "mb.css")

(define (racketfont* . args)
  (element 'tt args))

(define (fileblock filename . inside)
  (compound-paragraph 
   (style "fileblock" (list* (alt-tag "div") 'multicommand
                                 (box-mode "RfileboxBoxT" "RfileboxBoxC" "RfileboxBoxB") 
                                 scheme-properties))
   (list
    (paragraph (style "fileblock_filetitle" (list* (alt-tag "div") (box-mode* "RfiletitleBox") scheme-properties))
     (list (make-element
            (style "fileblock_filename" (list (css-style-addition mb-css)))
            (if (string? filename)
                (filepath filename)
                filename))))
    (compound-paragraph 
     (style "fileblock_filecontent" (list* (alt-tag "div") (box-mode* "RfilecontentBox") scheme-properties))
     (decode-flow inside)))))

(define (convert-newlines args)
  (map (λ(arg) (if (equal? arg "\n") (linebreak) arg)) args))

(define (repl-output . args)
  (nested (racketvalfont (racketfont* (convert-newlines args)))))

(define (errorblock . args)
  (nested (racketerror (racketfont* (convert-newlines args)))))

(define (browser . args)
  (compound-paragraph (style "browser" (list (css-style-addition mb-css) (alt-tag "div"))) (list (paragraph (style #f null) (string-append* args)))))

(define (terminal . args)
  (compound-paragraph (style "terminal" (list (css-style-addition mb-css) (alt-tag "div"))) (list (apply verbatim args))))


(define (noskip-note)
  (nested #:style (style "noskip" (list (css-style-addition mb-css) (alt-tag "div")))
         (margin-note "Don’t skip this section! It explains a concept that's essential to understanding how Pollen works.")))
  

(define-syntax (image/rp stx)
  (syntax-case stx ()
    [(_ name) #'(image/rp name 1.0)]
    [(_ name scale) #'(begin
                        (define-runtime-path rp name)
                        (image rp #:scale scale))]))