#lang scribble/manual

@(require scribble/eval pollen/cache pollen/setup (for-label racket (except-in pollen #%module-begin) pollen/render txexpr xml pollen/pagetree sugar/coerce pollen/template pollen/template/html pollen/setup))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/template pollen/template/html xml))

@title{Template}

@defmodule[pollen/template]

Convenience functions for templates. These are automatically imported into the @racket[eval] environment when rendering with a template (see @racket[render]).


@section{HTML}

@defmodule[pollen/template/html]

Functions specific to HTML templates.

@defproc[
(->html
[xexpr-or-xexprs (or/c xexpr? (listof xexpr?))]
[#:tag html-tag (or/c #f txexpr-tag?) #f]
[#:attrs html-attrs (or/c #f txexpr-attrs?) #f]
[#:splice? splice-html? boolean? #f])
string?]
Convert @racket[_xexpr-or-xexprs] to an HTML string. Similar to @racket[xexpr->string], but consistent with the HTML spec, text that appears within @code{script} or @code{style} blocks will not be escaped.

@examples[#:eval my-eval
(define tx '(root (script "3 > 2") "Why is 3 > 2?"))
(xexpr->string tx)
(->html tx)
]

The optional keyword arguments @racket[_html-tag] and @racket[_html-attrs] let you set the outer tag and attributes for the generated HTML. If @racket[_xexpr-or-xexprs] already has an outer tag or attributes, they will be replaced.

@examples[#:eval my-eval
(define tx '(root ((id "huff")) "Bunk beds"))
(->html tx)
(->html tx #:tag 'div)
(->html tx #:attrs '((id "doback")))
(->html tx #:tag 'div #:attrs '((id "doback")))
]

Whereas if @racket[_xexpr-or-xexprs] has no tag or attributes, they will be added. If you supply attributes without a tag, you'll get an error.

@examples[#:eval my-eval
(define x "Drum kit")
(->html x)
(->html x #:tag 'div)
(->html x #:tag 'div #:attrs '((id "doback")))
(->html x #:attrs '((id "doback")))
]


If the generated HTML has an outer tag, the @racket[_splice-html?] option will strip it off. Otherwise this option has no effect.

@examples[#:eval my-eval
(define tx '(root (p "Chicken nuggets")))
(->html tx)
(->html tx #:splice? #t)
(define x "Fancy sauce")
(->html x)
(code:comment @#,t{This next one won't do anything})
(->html x #:splice? #t)
(code:comment @#,t{Adds the outer tag, but then #:splice? removes it})
(->html x #:tag 'div #:attrs '((id "doback")) #:splice? #t)
]



Be careful not to pass existing HTML strings into this function, because the angle brackets will be escaped. Fine if that's what you want, but you probably don't.

@examples[#:eval my-eval
(define tx '(p "You did " (em "what?")))
(->html tx)
(->html (->html tx))
]

As the input contract suggests, this function can take either a single @racket[xexpr?] or a list of @racket[xexpr?], with the expected results.

@examples[#:eval my-eval
(define tx '(p "You did " (em "what?")))
(->html tx)
(define txs '("You " "did " (em "what?")))
(->html txs)
(->html #:tag 'p txs)
]
