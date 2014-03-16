#lang scribble/manual

@(require scribble/eval pollen/cache pollen/world (for-label racket (except-in pollen #%module-begin) pollen/template pollen/render xml pollen/pagetree))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/template xml))

@title{Template}

@defmodule[pollen/template]

Convenience functions for templates. These are automatically imported into the @racket[eval] environment when rendering with a template (see @racket[render]).

@defproc[
(->html
[xexpr xexpr?])
string?]
Convert @racket[_xexpr] to an HTML string. Similar to @racket[xexpr->string], but consistent with the HTML spec, text that appears within @code{script} or @code{style} blocks will not be escaped.

@examples[#:eval my-eval
(define tx '(root (script "3 > 2") "Why is 3 > 2?"))
(xexpr->string tx)
(->html tx)
]

Be careful not to pass existing HTML strings into this function, because the @code{<} and @code{>} symbols will be escaped. Fine if that's what you want, but you probably don't.

@examples[#:eval my-eval
(define tx '(p "You did" (em "what?")))
(->html tx)
(->html (->html tx))
]

@deftogether[(

@defproc[
(from
[query symbolish?]
[pagenode pagenodeish?])
(or/c #f txexpr-element?)]

@defproc[
(from*
[query symbolish?]
[pagenode pagenodeish?])
(or/c #f (listof txexpr-element?))]

)]
Find matches for @racket[_query] in @racket[_pagenode], first by looking in its @code{metas} (using @racket[from-metas]) and then by looking in its @code{doc} (using @racket[from-doc]). With @racket[from], you get the first result; with @racket[from*], you get them all. In both cases, you get @racket[#f] if there are no matches.




@defproc[
(from-metas
[query symbolish?]
[meta-source (or/c pagenodeish? hash?)])
(or/c #f txexpr-element?)]
Look up the value of @racket[_query] in @racket[_meta-source]. The @racket[_meta-source] argument can be either a set of metas (i.e., a @racket[hash]) or a @racket[pagenode?], from which metas are pulled. If no value exists for @racket[_query], you get @racket[#f].

@examples[#:eval my-eval
(define my-metas (hash 'template "sub.xml.pt" 'target "print"))
(from-metas 'template  my-metas)
('target . from-metas . my-metas)
(from-metas 'nonexistent-key my-metas)
]



@defproc[
(from-doc
[query symbolish?]
[doc-source (or/c pagenodeish? txexpr?)])
(or/c #f txexpr-element?)]
Look up the value of @racket[_query] in @racket[_doc-source]. The @racket[_doc-source] argument can be either be a @code{doc} (i.e., a @racket[txexpr]) or a @racket[pagenode?], from which doc is pulled. If no value exists for @racket[_query], you get @racket[#f].

@examples[#:eval my-eval
(define my-doc '(body (question "Gelato?") 
(answer "Nocciola") (answer "Pistachio")))
(from-doc 'question  my-doc)
('answer . from-doc . my-doc)
(from-doc 'nonexistent-key my-doc)
]


