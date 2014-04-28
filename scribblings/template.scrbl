#lang scribble/manual

@(require scribble/eval pollen/cache pollen/world (for-label racket (except-in pollen #%module-begin) pollen/render xml pollen/pagetree sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/template xml))

@title{Template}

@defmodule[pollen/template]

Convenience functions for templates. These are automatically imported into the @racket[eval] environment when rendering with a template (see @racket[render]).

This module also provides everything from @racket[sugar/coerce/value].

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

Be careful not to pass existing HTML strings into this function, because the angle brackets will be escaped. Fine if that's what you want, but you probably don't.

@examples[#:eval my-eval
(define tx '(p "You did" (em "what?")))
(->html tx)
(->html (->html tx))
]

@deftogether[(

@defproc[
(select
[key symbolish?]
[value-source (or/c hash? txexpr? pagenode? pathish?)])
(or/c #f txexpr-element?)]

@defproc[
(select*
[key symbolish?]
[value-source (or/c hash? txexpr? pagenode? pathish?)])
(or/c #f (listof txexpr-element?))]

)]
Find matches for @racket[_key] in @racket[_value-source], first by looking in its @code{metas} (using @racket[select-from-metas]) and then by looking in its @code{doc} (using @racket[select-from-doc]). With @racket[select], you get the first result; with @racket[select*], you get them all. In both cases, you get @racket[#f] if there are no matches.




@defproc[
(select-from-metas
[key symbolish?]
[meta-source (or/c hash? pagenodeish? pathish?)])
(or/c #f txexpr-element?)]
Look up the value of @racket[_key] in @racket[_meta-source]. The @racket[_meta-source] argument can be either a set of metas (i.e., a @racket[hash]) or a @racket[pagenode?], from which metas are pulled. If no value exists for @racket[_key], you get @racket[#f].

@examples[#:eval my-eval
(module ice-cream pollen/markup
'(div (question "Flavor?")
  (answer "Chocolate chip") (answer "Maple walnut"))
  '(meta ((template "sub.xml.pt")))
  '(meta ((target "print"))))
(code:comment @#,t{Import doc & metas from 'ice-cream submodule})
(require 'ice-cream)
(select-from-metas 'template  metas)
('target . select-from-metas . metas)
(select-from-metas 'nonexistent-key metas)
]



@defproc[
(select-from-doc
[key symbolish?]
[doc-source (or/c txexpr? pagenodeish? pathish?)])
(or/c #f txexpr-element?)]
Look up the value of @racket[_key] in @racket[_doc-source]. The @racket[_doc-source] argument can be either be a @code{doc} (i.e., a @racket[txexpr]) or a @racket[pagenode?], from which doc is pulled. If no value exists for @racket[_key], you get @racket[#f].

@examples[#:eval my-eval
(module gelato pollen/markup
'(div (question "Flavor?")
  (answer "Nocciola") (answer "Pistachio"))
  '(meta ((template "sub.xml.pt")))
  '(meta ((target "print"))))
(code:comment @#,t{Import doc & metas from 'gelato submodule})
(require 'gelato)
(select-from-doc 'question  doc)
('answer . select-from-doc . doc)
(select-from-doc 'nonexistent-key doc)
]


