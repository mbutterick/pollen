#lang scribble/manual

@(require scribble/eval pollen/cache pollen/world (for-label racket (except-in pollen #%module-begin) pollen/world pollen/pagemap txexpr pollen/decode))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/pagemap txexpr))

@title{Pagemaps}

@defmodule[pollen/pagemap]

A @italic{pagemap} is a hierarchical list of Pollen output files. A pagemap source file has the extension @code[(format ".~a" world:pagemap-source-ext)]. A pagemap provides a convenient way of separating the structure of the pages from the page sources, and navigating around this structure.

Books and other long documents are usually organized in a structured way — at minimum they have a sequence of pages, but more often they have sections with subsequences within. Individual Pollen source files don't know anything about how they're connected to other files. In theory, you could maintain this information within each source file. This would be a poor use of human energy. Let the pagemap figure it out.

@defproc[
(pagemap?
[possible-pagemap any/c])
boolean?]
Test whether @racket[_possible-pagemap] is a valid pagemap. It must be a @racket[txexpr?] where all elements are @racket[node?] and unique within @racket[_possible-pagemap] (not counting the root node).

@examples[#:eval my-eval
(pagemap? '(root index.html))
(pagemap? '(root index.html index.html))
(pagemap? '(root index.html "index.html"))
(define nested-pmap '(root 1.html 2.html (3.html 3a.html 3b.html)))
(pagemap? nested-pmap)
(pagemap? `(root index.html ,nested-pmap (subsection.html more.html)))
(pagemap? `(root index.html ,nested-pmap (subsection.html ,nested-pmap)))
]

@defproc[
(validate-pagemap
[possible-pagemap any/c])
pagemap?]
Like @racket[pagemap?], but raises a descriptive error if @racket[_possible-pagemap] is invalid, and otherwise returns @racket[_possible-pagemap] itself.

@examples[#:eval my-eval
(validate-pagemap '(root (mama.html son.html daughter.html) uncle.html))
(validate-pagemap `(root (,+ son.html daughter.html) uncle.html))
(validate-pagemap '(root (mama.html son.html son.html) mama.html))
]


@defproc[
(node?
[possible-node any/c])
boolean?]
Test whether @racket[_possible-node] is a valid node (short for ``pagemap node''). A node can be any @racket[symbol?] that is not @racket[whitespace/nbsp?] Every leaf of a pagemap is a node. In practice, your nodes will likely be names of output files. 

@margin-note{Nodes are symbols (rather than strings) so that pagemaps will be valid tagged X-expressions, which is a more convenient format for validation & processing.}

@examples[#:eval my-eval
(map node? '(symbol index.html |   silly   |))
(map node? '(9.999 "index.html" (p "Hello") |    |))
]


@defproc[
(nodeish?
[v any/c])
boolean?]
Return @racket[#t] if @racket[_v] can be converted with @racket[->node].

@examples[#:eval my-eval
(map nodeish? '(9.999 "index.html" |    |))
]


@defproc[
(->node
[v nodeish?])
node?]
Convert @racket[_v] to a node.

@examples[#:eval my-eval
(map nodeish? '(symbol 9.999 "index.html" |  silly  |))
(map ->node '(symbol 9.999 "index.html" |  silly  |))
]



@section{Navigation}


@defparam[current-pagemap pagemap pagemap?
          #:value #f]{
A parameter that defines the default pagemap used by pagemap navigation functions (e.g., @racket[parent], @racket[chidren], et al.) if another is not explicitly specified. Initialized to @racket[#f].}


@defproc[
(parent
[p (or/c #f nodeish?)]
[pagemap pagemap? (current-pagemap)])
(or/c #f node?)]
Find the parent node of @racket[_p] within @racket[_pagemap]. Return @racket[#f] if there isn't one.

@examples[#:eval my-eval
(current-pagemap '(root (mama.html son.html daughter.html) uncle.html))
(parent 'son.html)
(parent "mama.html")
(parent (parent 'son.html))
(parent (parent (parent 'son.html)))
]

@defproc[
(children
[p (or/c #f nodeish?)]
[pagemap pagemap? (current-pagemap)])
(or/c #f node?)]
Find the child nodes of @racket[_p] within @racket[_pagemap]. Return @racket[#f] if there aren't any.

@examples[#:eval my-eval
(current-pagemap '(root (mama.html son.html daughter.html) uncle.html))
(children 'mama.html)
(children 'uncle.html)
(children 'root)
(map children (children 'root))
]


@defproc[
(siblings
[p (or/c #f nodeish?)]
[pagemap pagemap? (current-pagemap)])
(or/c #f node?)]
Find the sibling nodes of @racket[_p] within @racket[_pagemap]. The list will include @racket[_p] itself. But the function will still return @racket[#f] if @racket[_pagemap] is @racket[#f].

@examples[#:eval my-eval
(current-pagemap '(root (mama.html son.html daughter.html) uncle.html))
(siblings 'son.html)
(siblings 'daughter.html)
(siblings 'mama.html)
]


@deftogether[(

@defproc[
(previous
[p (or/c #f nodeish?)]
[pagemap pagemap? (current-pagemap)])
(or/c #f node?)]

@defproc[
(previous*
[p (or/c #f nodeish?)]
[pagemap pagemap? (current-pagemap)])
(or/c #f (listof node?))]
)]
Return the node immediately before @racket[_p]. For @racket[previous*], return all the nodes before @racket[_p], in sequence. In both cases, return @racket[#f] if there aren't any nodes. The root node is ignored.

@examples[#:eval my-eval
(current-pagemap '(root (mama.html son.html daughter.html) uncle.html))
(previous 'daughter.html)
(previous 'son.html)
(previous (previous 'daughter.html))
(previous 'mama.html)
(previous* 'daughter.html)
(previous* 'uncle.html)
]

@deftogether[(

@defproc[
(next
[p (or/c #f nodeish?)]
[pagemap pagemap? (current-pagemap)])
(or/c #f node?)]

@defproc[
(next*
[p (or/c #f nodeish?)]
[pagemap pagemap? (current-pagemap)])
(or/c #f (listof node?))]
)]
Return the node immediately after @racket[_p]. For @racket[next*], return all the nodes after @racket[_p], in sequence. In both cases, return @racket[#f] if there aren't any nodes. The root node is ignored.

@examples[#:eval my-eval
(current-pagemap '(root (mama.html son.html daughter.html) uncle.html))
(next 'son.html)
(next 'daughter.html)
(next (next 'son.html))
(next 'uncle.html)
(next* 'mama.html)
(next* 'daughter.html)
]

