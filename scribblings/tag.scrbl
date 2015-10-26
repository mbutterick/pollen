#lang scribble/manual

@(require scribble/eval pollen/cache pollen/world (for-label racket (except-in pollen #%module-begin) txexpr pollen/tag pollen/render xml pollen/pagetree))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/template xml))

@title{Tag}

@defmodule[pollen/tag]

Convenience functions for working with tags.


@defproc[
(make-default-tag-function
[id txexpr-tag?]
[kw-attr-name keyword?]
[kw-attr-value string?] ... ...)
(-> txexpr?)]
Make a default tag function for @racket[_id]. The new tag function takes an optional set of X-expression attributes (@racket[txexpr-attrs?]) followed by X-expression elements (@racket[txexpr-elements?]). From these, the tag function creates a tagged X-expression using @racket[_id] as the tag.

@examples[
(require pollen/tag)
(define beaucoup (make-default-tag-function 'em))
(beaucoup "Bonjour")
(beaucoup '((id "greeting")) "Bonjour")
]

Entering attributes this way can be cumbersome. So for convenience, the new tag function provides an alternative: any keyword arguments and their values will be interpreted as attributes.

@examples[
(require pollen/tag)
(define beaucoup (make-default-tag-function 'em))
(beaucoup #:id "greeting" #:class "large" "Bonjour")
]

You can also provide keyword arguments to @racket[make-default-tag-function] itself, and they will become default attributes for every use of the tag function.

@examples[
(require pollen/tag)
(define beaucoup-small (make-default-tag-function 'em #:class "small"))
(beaucoup-small #:id "greeting" "Bonjour")
]

Pollen also uses this function to provide the default behavior for undefined tags. See @racket[#%top].

Note that while default tag functions are typically used to generate tagged X-expressions, they don't enforce any restrictions on input, so they also do not guarantee that you will in fact get a valid tagged X-expression as output. This is intentional — default tag functions are a coding convenience, and their output is likely to be processed by other tag functions, so raising the error here would be premature.

@examples[
(require pollen/tag)
(define strange (make-default-tag-function 'div #:class "bizarre"))
(code:comment @#,t{Invalid data types for elements})
(strange + *)
(code:comment @#,t{Double "class" attribute})
(strange #:class "spooky")
]



@defproc[
(split-attributes
[parts list?])
(values txexpr-attrs? txexpr-elements?)]
Helper function for custom tag functions. Take a rest argument that possibly includes tag attributes plus elements, and split it into attributes and elements. If there are no attributes, that return value will be the empty list. Properly parses the abbreviated Pollen syntax for attributes (described in @racket[make-default-tag-function]).

@examples[
(require pollen/tag)
(define (tag . parts) 
  (define-values (attrs elements) (split-attributes parts))
  (values attrs elements))
(tag "Hello world")
(tag '((key "value")) "Hello world")
(tag 'key: "value" "Hello world")
]