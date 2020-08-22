#lang at-exp racket/base
(require (for-syntax racket/base
                     racket/syntax)
         racket/format
         racket/runtime-path
         scribble/core
         scribble/decode
         scribble/html-properties
         scribble/latex-properties
         scribble/manual
         scribble/private/manual-sprop)


(provide (all-defined-out) (all-from-out racket/runtime-path))

(define-runtime-path mb-css "mb.css")
(define-runtime-path mb-tex "mb.tex")

(define (link-tt url) (link url (tt url)))

(define (racketfont* . args)
  (element 'tt args))

(define (fileblock filename . inside)
  (compound-paragraph 
   (style "fileblock" (list* (alt-tag "div") 'multicommand
                             (box-mode "RfileboxBoxT" "RfileboxBoxC" "RfileboxBoxB")
                             (tex-addition mb-tex)
                             scheme-properties))
   (list
    (paragraph (style "fileblockFiletitle" (list* (alt-tag "div") (box-mode* "RfiletitleBox") (tex-addition mb-tex) scheme-properties))
               (list (make-element
                      (style "fileblockFilename" (list (css-style-addition mb-css) (tex-addition mb-tex)))
                      (if (string? filename)
                          (filepath filename)
                          filename))))
    (compound-paragraph 
     (style "fileblockFilecontent" (list* (alt-tag "div") (box-mode* "RfilecontentBox") (tex-addition mb-tex) scheme-properties))
     (decode-flow inside)))))

(define (convert-newlines args)
  (map (λ (arg) (if (equal? arg "\n") (linebreak) arg)) args))

(define (repl-output . args)
  (nested (racketvalfont (racketfont* (convert-newlines args)))))

(define (errorblock . args)
  (nested (racketerror (racketfont* (convert-newlines args)))))

(define (foreign-code . args)
  (compound-paragraph (style "foreignCode" (list (css-style-addition mb-css)
                                                 (alt-tag "div")
                                                 (tex-addition mb-tex)))
                      (list (apply verbatim args))))

(define (terminal . args)
  (compound-paragraph (style "terminal" (list (css-style-addition mb-css)
                                              (alt-tag "div")
                                              (tex-addition mb-tex)))
                      (list (apply verbatim args))))

(define (browser . args)
  (compound-paragraph (style "browser" (list (css-style-addition mb-css)
                                             (alt-tag "div")
                                             (tex-addition mb-tex)))
                      (list (paragraph (style #f null) (convert-newlines args)))))


(define (noskip-note)
  (nested #:style (style "noskip" (list (css-style-addition mb-css)
                                        (alt-tag "div")
                                        (tex-addition mb-tex)))
          (margin-note "Don’t skip this section! It explains an essential Pollen concept.")))


(define-syntax (image/rp stx)
  (syntax-case stx ()
    [(_ name xs ...)
     (with-syntax ([id (generate-temporary)])
       #'(begin
           (define-runtime-path id name)
           (image id xs ...)))]))


(define (val . args)
  (racketvalfont (element 'tt (map ~v args))))

(define (id . args)
  (element 'tt (map ~a args)))

(define (ext expr)
  @code[(format ".~a" expr)])
