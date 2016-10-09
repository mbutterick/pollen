#lang scribble/manual

@(require scribble/eval pollen/decode pollen/setup txexpr (for-label pollen/unstable/convert pollen/setup txexpr racket (except-in pollen #%module-begin)))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/unstable/convert))
@(require "mb-tools.rkt")


@title{Convert}

@defmodule[pollen/unstable/convert]

Helper functions for converting well-made X-expressions or HTML into Pollen markup.

@defproc[
(xexpr->pollen
[x xexpr?]
[#:white-p? white-p? boolean? #f]
)
string?]
Convert @racket[_x] to Pollen markup, using @racket[setup:command-char] as the leading character. If @racket[_white-p?] is @racket[#t], convert @racket[p] tags by taking the content out of the tag, and inserting @racket[setup:paragraph-separator] in front.

@examples[#:eval my-eval
(xexpr->pollen '(div))
(xexpr->pollen '(div ((class "pups"))))
(xexpr->pollen '(div ((class "pups")) "Lex"))
(xexpr->pollen '(div ((class "pups")) (p "Roxy") (p "Sidney")))
(xexpr->pollen #:white-p? #t '(div ((class "pups")) (p "Roxy") (p "Sidney")))
]

@defproc[
(html->pollen
[html string?]
[#:white-p? white-p? boolean? #f]
)
string?]
Convert @racket[_html] to Pollen markup, using @racket[setup:command-char] as the leading character. If @racket[_white-p?] is @racket[#t], convert @racket[p] tags by taking the content out of the tag, and inserting @racket[setup:paragraph-separator] in front. 

@examples[#:eval my-eval
(html->pollen "<hr />")
(html->pollen "<div class=\"pups\" />")
(html->pollen "<div class=\"pups\">Lex</div>")
(html->pollen "<div class=\"pups\"><p>Roxy</p><p>Sidney</p></div>")
(html->pollen #:white-p? #t "<div class=\"pups\"><p>Roxy</p><p>Sidney</p></div>")
]

This function treats @racket[_html] as an XML-ish data structure. Meaning, @racket[_html] is expected to be syntactically well-formed (in terms of attributes and tags). But it doesn't need to comply with a certain HTML specification. This casual approach suffices in most cases. Be aware, however, that this function won't handle HTML @racket[style] or @racket[script] blocks correctly, if they contain reserved XML characters.

@examples[#:eval my-eval
(html->pollen "<script type=\"javascript\">x < 3</script>")
(html->pollen "<style type=\"css\">div{content: \"<&>\";}</style>")
]

But if the interiors of the @racket[style] or @racket[script] blocks are wrapped with @racket[CDATA] designations, they will convert correctly: 

@examples[#:eval my-eval
(html->pollen "<script type=\"javascript\"><![CDATA[x < 3]]></script>")
(html->pollen "<style type=\"css\"><![CDATA[div{content: \"<&>\";}]]></style>")
]

@defproc[
(url->pollen
[url (or/c string? url?)]
[#:white-p? white-p? boolean? #f]
)
string?]
Like @racket[html->pollen], but takes a @racket[_url] as input and fetches its HTML.
