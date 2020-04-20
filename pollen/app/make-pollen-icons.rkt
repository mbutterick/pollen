#lang br
(require pict icns file/ico rsvg)

(define p (svg-file->pict "pollen-raw.svg"))
(send (pict->bitmap p) save-file "pollen.png" 'png)

(with-output-to-file "pollen.icns"
  (λ () (void (write-bytes (pict->icns-bytes p))))
  #:exists 'replace)

(write-icos (for/list ([size '(16 32 48 256)])
              (argb->ico size size (pict->argb-pixels (scale p (/ size 720))))) "pollen.ico"
                                                           #:exists 'replace)