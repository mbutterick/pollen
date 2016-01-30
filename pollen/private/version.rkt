#lang racket/base
(require racket/file racket/format racket/runtime-path)
(provide pollen:version)
(define-runtime-path ts-file "ts.rktd")
(define ts (file->value ts-file))

(define (convert str)
  (apply string-append
         (for/list ([c (in-string str)])
                   (define c-int (char->integer c))
                   (~a (integer->char (+ c-int (if (<= (char->integer #\0) c-int (char->integer #\9))
                                                   (- (char->integer #\a) (char->integer #\0))
                                                   10)))))))

(define (get-version)
  (define major-version 0)
  (define minor-version (convert (~r ts #:base 26)))
  (format "~a.~a" major-version minor-version))


(define pollen:version (get-version))