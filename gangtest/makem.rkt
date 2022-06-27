#lang br
(require racket/file)

(for ([i 50])
     (display-to-file (format "#lang pollen\n~a file" i)
                      (string->path (format "~a.txt.pm" i))
                      #:exists 'replace
                      #:mode 'text))