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
  (define minor-version 9)
  (define pollen-birthday 1375340400) ; 8/1/2013, 12:00:00 AM GMT-7:00 DST
  (define seconds-per-year (* 60 60 24))
  (define build-time (- (file->value ts-file) pollen-birthday))
  (define-values (build-days build-seconds) (quotient/remainder build-time seconds-per-year))
  (format "~a.~a.~a.~a" major-version minor-version build-days build-seconds))


(define pollen:version (get-version))