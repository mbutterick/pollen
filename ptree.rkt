#lang racket/base
(require racket/contract)
(require "tools.rkt" "world.rkt" "ptree-nav.rkt" "ptree-decode.rkt" "debug.rkt")

(module+ test (require rackunit))

(provide (all-defined-out) (all-from-out "ptree-nav.rkt"))


;; function to set up the project-ptree.
;; this is to make life simpler when using tree navigation functions.
;; the current main.ptree of the project is used as the default input.
;; without this, you'd have to pass it over and over.
;; which is sort of the functional lifestyle, 
;; but in templates, gets tiresome and error-prone.
(define/contract (make-project-ptree)
  (-> ptree?)
  (define ptree-source (build-path START_DIR DEFAULT_POLLEN_TREE))
  (if (file-exists? ptree-source)
      ;; Load it from default path.
      ;; dynamic require of a ptree source file gets you a full ptree. 
      (begin
        (message "Using ptree file" (->string (file-name-from-path ptree-source)))
        (dynamic-require ptree-source POLLEN_ROOT))
      ;; ... or else synthesize it
      (let* ([files (directory-list START_DIR)]
             ;; restrict files to those with pollen extensions
             [files (map remove-ext (filter (λ(x) (has-ext? x POLLEN_SOURCE_EXT)) files))])
        ;; make a POLLEN_TREE_ROOT_NAME structure and convert it to a full ptree
        (message "Generating ptree from file listing")
        (ptree-root->ptree (cons POLLEN_TREE_ROOT_NAME (map path->pnode files))))))

(set-project-ptree (make-project-ptree))
