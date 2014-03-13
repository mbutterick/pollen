#lang scribble/manual

@(require scribble/eval pollen/cache pollen/world (for-label racket (except-in pollen #%module-begin) pollen/template pollen/render xml))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/template xml))

@title{Template}

@defmodule[pollen/template]

Convenience functions for templates. These are automatically imported into the @racket[eval] environment when rendering with a template (see @racket[render]).

@defproc[
(->html
[tx txexpr?])
string?]
Convert @racket[_tx] to an HTML string. Consistent with the HTML spec (and unlike @racket[xexpr->string]), text that appears within @code{script} or @code{style} blocks will not be escaped.

@examples[#:eval my-eval
(define tx '(root (script "3 > 2") "Why is 3 > 2?"))
(xexpr->string tx)
(->html tx)
]
