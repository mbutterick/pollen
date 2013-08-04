#lang racket
(require (for-syntax (planet mb/pollen/tools)
                     (planet mb/pollen/world)))
(require (planet mb/pollen/tools)
         (planet mb/pollen/world))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Look for a EXTRAS_DIR directory local to the source file.
;; If it exists, get list of rkt files 
;; and require + provide them.
;; This will be resolved in the context of current-directory.
;; So when called from outside the project directory, 
;; current-directory must be properly set with 'parameterize'

(require racket/contract/region)

(define-for-syntax (is-rkt-file? x) (has-ext? x 'rkt))

(define-for-syntax (make-complete-path x)
  (define-values (start_dir name _ignore) 
    (split-path (path->complete-path x)))
  (build-path start_dir EXTRAS_DIR name))

(define-syntax (require-and-provide-extras stx)
  (if (directory-exists? EXTRAS_DIR)
      (letrec 
          ([files (map make-complete-path (filter is-rkt-file? (directory-list EXTRAS_DIR)))]
           [files-in-require-form 
            (map (λ(x) `(file ,(path->string x))) files)]) 
        (datum->syntax stx 
                       `(begin
                          (require ,@files-in-require-form)
                          (provide (all-from-out ,@files-in-require-form)))))
      ; if no files to import, do nothing
      #'(begin))) ; tried (void) here but it doesn't work: prints <void>


; todo: merge with function above
(define-syntax (require-extras stx)
  (if (directory-exists? EXTRAS_DIR)
      (letrec 
          ([files (map make-complete-path (filter is-rkt-file? (directory-list EXTRAS_DIR)))]
           [files-in-require-form 
            (map (λ(x) `(file ,(path->string x))) files)]) 
        (datum->syntax stx 
                       `(begin
                          (require ,@files-in-require-form))))
      ; if no files to import, do nothing
      #'(begin)))


; AHA! This is how to make an identifier secretly behave as a runtime function
; first, define the function as syntax-rule
(define-syntax-rule (get-here)
  (begin ; define-syntax-rule must have a single expression in the body
    ; also, even though begin permits defines,
    ; macro might be used in an expression context, whereupon they will cause an error.
    ; so best to use let
    (let ([ccr (current-contract-region)]) ; trick for getting current module name
      (when (list? ccr) ; if contract-region is called from within submodule, you get a list
        (set! ccr (car ccr)))  ; in which case, just grab the path from the front
      (if (equal? 'pollen-lang-module ccr) ; what happens if the file isn't yet saved in drracket
          'nowhere ; thus you are nowhere
          (match-let-values ([(_ here-name _) (split-path ccr)])
                            (path->string (remove-all-ext here-name)))))))

; then, apply a separate syntax transform to the identifier itself
; can't do this in one step, because if the macro goes from identifier to function definition,
; macro processor will evaluate the body at compile-time, not runtime.
(define-syntax here
  (λ(stx) (datum->syntax stx '(get-here))))


; function to strip metas out of body and consolidate them separately
(define (split-metas body)
  (define meta-list '())
  (define (&split-metas x)
    (cond
      [(and (named-xexpr? x) (equal? 'meta (car x)))
       (begin
         (set! meta-list (cons x meta-list))
         empty)]
      [(named-xexpr? x) ; handle named-xexpr
       (let-values([(name attr body) (xexplode x)]) 
         (make-xexpr name attr (&split-metas body)))]
      [(list? x) (map &split-metas x)]
      [else x]))
  (values (remove-empty (&split-metas body)) (reverse meta-list))) 

(provide (all-defined-out))

(module+ test
  (require rackunit)
  (check-equal? (get-here) "test-main-helper"))