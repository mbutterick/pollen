#lang scribble/manual

@(require scribble/eval pollen/cache pollen/world (for-label racket (except-in pollen #%module-begin) pollen/render txexpr xml pollen/pagetree sugar/coerce))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/template xml))

@title{Template}

@defmodule[pollen/template]

Convenience functions for templates. These are automatically imported into the @racket[eval] environment when rendering with a template (see @racket[render]).

This module also provides everything from @racketmodname[sugar/coerce].

@defproc[
(->html
[xexpr xexpr?]
[#:tag html-tag (or/c #f txexpr-tag?) #f]
[#:attrs html-attrs (or/c #f txexpr-attrs?) #f]
[#:splice splice-html? boolean? #f])
string?]
Convert @racket[_xexpr] to an HTML string. Similar to @racket[xexpr->string], but consistent with the HTML spec, text that appears within @code{script} or @code{style} blocks will not be escaped.

@examples[#:eval my-eval
(define tx '(root (script "3 > 2") "Why is 3 > 2?"))
(xexpr->string tx)
(->html tx)
]

The optional keyword arguments @racket[_html-tag] and @racket[_html-attrs] let you set the outer tag and attributes for the generated HTML. If @racket[_xexpr] already has an outer tag or attributes, they will be replaced.

@examples[#:eval my-eval
(define tx '(root ((id "huff")) "Bunk beds"))
(->html tx)
(->html tx #:tag 'div)
(->html tx #:attrs '((id "doback")))
(->html tx #:tag 'div #:attrs '((id "doback")))
]

Whereas if @racket[_xexpr] has no tag or attributes, they will be added. If you supply attributes without a tag, you'll get an error.

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
(->html tx #:splice #t)
(define x "Fancy sauce")
(->html x)
(code:comment @#,t{This next one won't do anything})
(->html x #:splice #t)
(code:comment @#,t{Adds the outer tag, but then #:splice removes it})
(->html x #:tag 'div #:attrs '((id "doback")) #:splice #t)
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

@defproc[
(when/block
[condition any/c]
[text-to-insert any/c])
string?]
Convenience function for templates that's simpler to use than plain @racket[when]. If @racket[_condition] is true, then put the @racket[_text-to-insert] into the template at the current location. Within a template file, usually invoked like so:

@verbatim{◊when/block[@racketvarfont{condition}]{The text to insert.}}

The inserted text can contain its own nested Pollen commands.

