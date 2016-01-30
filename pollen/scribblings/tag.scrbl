#lang scribble/manual

@(require scribble/eval pollen/cache pollen/setup (for-label racket (except-in pollen #%module-begin) txexpr pollen/tag pollen/render xml pollen/pagetree))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/template xml))

@title{Tag}

@defmodule[pollen/tag]

Convenience functions for working with tags.


@defproc[
(default-tag-function
[id txexpr-tag?]
[kw-attr-name keyword?]
[kw-attr-value string?] ... ...)
(-> txexpr?)]
Make a default tag function for @racket[_id]. The new tag function takes an optional set of X-expression attributes (@racket[txexpr-attrs?]) followed by X-expression elements (@racket[txexpr-elements?]). From these, the tag function creates a tagged X-expression using @racket[_id] as the tag.

@examples[
(require pollen/tag)
(define beaucoup (default-tag-function 'em))
(beaucoup "Bonjour")
(beaucoup '((id "greeting")) "Bonjour")
]

Entering attributes this way can be cumbersome. So for convenience, the new tag function provides an alternative: any keyword arguments and their values will be interpreted as attributes.

@examples[
(require pollen/tag)
(define beaucoup (default-tag-function 'em))
(beaucoup #:id "greeting" #:class "large" "Bonjour")
]

You can also provide keyword arguments to @racket[default-tag-function] itself, and they will become default attributes for every use of the tag function.

@examples[
(require pollen/tag)
(define beaucoup-small (default-tag-function 'em #:class "small"))
(beaucoup-small #:id "greeting" "Bonjour")
]

Pollen also uses this function to provide the default behavior for undefined tags. See @racket[#%top].

Note that while default tag functions are typically used to generate tagged X-expressions, they don't enforce any restrictions on input, so they also do not guarantee that you will in fact get a valid tagged X-expression as output. This is intentional — default tag functions are a coding convenience, and their output is likely to be processed by other tag functions, so raising the error here would be premature.

@examples[
(require pollen/tag)
(define strange (default-tag-function 'div #:class "bizarre"))
(code:comment @#,t{Invalid data types for elements})
(strange + *)
(code:comment @#,t{Double "class" attribute})
(strange #:class "spooky")
]



@defform[
(define-tag-function
(tag-id attr-id elem-id) body ...)]
Helper function for making custom tag functions. Handles parsing chores, including conversion of keyword arguments into attributes (described in @racket[default-tag-function]), and parses other attributes and elements normally.

@examples[
(require pollen/tag)
(define-tag-function (tag-name attrs elems) 
  `(new-name ,(cons '(zim "zam") attrs) ,@elems))
(tag-name "Hello world")
(tag-name '((key "value")) "Hello world")
(tag-name #:key "value" "Hello world")
]