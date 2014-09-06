#lang scribble/manual

@(require scribble/eval pollen/cache pollen/world (for-label racket (except-in pollen #%module-begin) txexpr pollen/tag pollen/render xml pollen/pagetree))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/template xml))

@title{Tag}

@defmodule[pollen/tag]

Convenience functions for working with tags.


@defproc[
(make-default-tag-function
[id txexpr-tag?])
(-> txexpr?)]
Make a default tag function for @racket[_id]. As arguments, a tag function takes an optional set of X-expression attributes (@racket[txexpr-attrs?]) followed by X-expression elements (@racket[txexpr-elements?]). From these, the tag function creates a tagged X-expression using @racket[_id] as the tag.

@examples[
(require pollen/tag)
(define beaucoup (make-default-tag-function 'em))
(beaucoup "Bonjour")
(beaucoup '((id "greeting")) "Bonjour")
]

Entering attributes this way can be cumbersome. So for convenience, a tag function provides an alternative: any symbol + string pairs at the front of your expression will be interpreted as attributes, if the symbols are followed by a colon. If you leave out the colon, the symbols will be interpreted as part of the content of the tag.

@examples[
(require pollen/tag)
(define beaucoup (make-default-tag-function 'em))
(beaucoup 'id: "greeting" 'class: "large" "Bonjour")
(code:comment @#,t{Don't forget the colons})
(beaucoup 'id "greeting" 'class "large" "Bonjour")
(code:comment @#,t{Don't forget to provide a value for each attribute})
(beaucoup 'id: 'class: "large" "Bonjour")
]

Pollen also uses this function to provide the default behavior for undefined tags. See @racket[#%top].

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