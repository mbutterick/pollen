#lang racket/base
(require (for-syntax racket/base racket/syntax) scribble/core scribble/base scribble/manual racket/list scribble/private/manual-sprop scribble/decode scribble/html-properties racket/runtime-path racket/string)

(provide (all-defined-out) (all-from-out racket/runtime-path))

(define-runtime-path mb-css "mb.css")

(define (link-tt url) (link url (tt url)))

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



(define (terminal . args)
  (compound-paragraph (style "terminal" (list (css-style-addition mb-css) (alt-tag "div"))) (list (apply verbatim args))))

(define (browser . args)
  (compound-paragraph (style "browser" (list (css-style-addition mb-css) (alt-tag "div"))) (list (paragraph (style #f null) (convert-newlines args)))))


(define (noskip-note)
  (nested #:style (style "noskip" (list (css-style-addition mb-css) (alt-tag "div")))
         (margin-note "Don’t skip this section! It explains a concept that's essential to understanding how Pollen works.")))
  

(define-syntax (image/rp stx)
 (syntax-case stx ()
   [(_ name xs ...)
    (with-syntax ([id (generate-temporary)])
      #'(begin
          (define-runtime-path id name)
          (image id xs ...)))]))