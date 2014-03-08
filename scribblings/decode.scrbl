#lang scribble/manual

@(require scribble/eval pollen/decode pollen/world (for-label racket (except-in pollen #%module-begin) pollen/world pollen/cache pollen/decode txexpr xml pollen/predicates pollen/decode/typography pollen/decode/block))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/decode pollen/decode/typography pollen/decode/block))

@section{Decode}

@defmodule[pollen/decode]

@defproc[
(decode
[tagged-xexpr txexpr?]
[#:txexpr-tag-proc txexpr-tag-proc (txexpr-tag? . -> . txexpr-tag?) (λ(tag) tag)]
[#:txexpr-attrs-proc txexpr-attrs-proc (txexpr-attrs? . -> . txexpr-attrs?) (λ(attrs) attrs)]
[#:txexpr-elements-proc txexpr-elements-proc (txexpr-elements? . -> . txexpr-elements?) (λ(elements) elements)]
[#:block-txexpr-proc block-txexpr-proc (block-txexpr? . -> . block-txexpr?) (λ(tx) tx)]
[#:inline-txexpr-proc inline-txexpr-proc (txexpr? . -> . txexpr?) (λ(tx) tx)]
[#:string-proc string-proc (string? . -> . string?) (λ(str) str)]
[#:symbol-proc symbol-proc (symbol? . -> . symbol?) (λ(sym) sym)]
[#:valid-char-proc valid-char-proc (valid-char? . -> . valid-char?) (λ(vc) vc)]
[#:cdata-proc cdata-proc (cdata? . -> . cdata?) (λ(cdata) cdata)]
[#:exclude-tags tags-to-exclude (listof symbol?) null]
)
txexpr?]
Recursively process a @racket[_tagged-xexpr], usually the one exported from a Pollen source file as @racket['doc]. This function doesn't do much on its own. Rather, it provides the hooks upon which harder-working functions can be hung.

@margin-note{This is different from the Scribble approach, where the decoding logic is fixed for every document. In Pollen, you only get the decoding you ask for, and you can customize it to any degree.}

By default, the @racket[_tagged-xexpr] from a source file is tagged with @racket[root]. Recall from @secref{Pollen mechanics} that any tag can have a function attached to it. So the typical way to use @racket[decode] is to attach your decoding functions to it, and then define @racket[root] to invoke your @racket[decode] function. Then it will be automatically applied to every @racket['doc] during compile.

While @racket[decode] presents an imposing list of arguments, you're unlikely to use all of them at once. These represent possibilities, not requirements. Let's see what happens when @racket[decode] is invoked without any of its optional arguments:

@examples[#:eval my-eval
(define tx '(root "I wonder" (em "why") "this works."))
(decode tx)
]

Right — nothing. That's because the default value for the decoding arguments is the identity function, @racket[(λ(x)x)]. So everything gets passed through intact, until other action is specified.

The @racket[_txexpr-tag-proc] argument is a procedure that handles X-expression tags.

@examples[#:eval my-eval
(define tx '(p "I'm from a strange" (strong "namespace")))
(decode tx #:txexpr-tag-proc (λ(t) (string->symbol (format "ns:~a" t))))
]

The @racket[_txexpr-attrs-proc] argument is a procedure that handles lists of X-expression attributes. (The @racket[txexpr] module, included at no extra charge with Pollen, includes useful helper functions for dealing with attribute lists.)

@examples[#:eval my-eval
(define tx '(p [[id "first"]] "If I only had a brain."))
(decode tx #:txexpr-attrs-proc (λ(attrs) (cons '[class "PhD"] attrs )))
]

Note that @racket[_txexpr-attrs-proc] will change the attributes of every tagged X-expression, even those that don't have attributes. This is useful, because sometimes you want to add attributes where none existed before. But be careful, because the behavior may make your processing function overinclusive.

@examples[#:eval my-eval
(define tx '(div (p [[id "first"]] "If I only had a brain.") 
(p "Me too.")))
(decode tx #:txexpr-attrs-proc (λ(attrs) (cons '[class "PhD"] attrs )))
(decode tx #:txexpr-attrs-proc 
(λ(attrs) (if (null? attrs) attrs (cons '[class "PhD"] attrs ))))
]


The @racket[_txexpr-elements-proc] argument is a procedure that operates on the list of elements that represents the content of each tagged X-expression. Note that each element of an X-expression is subject to two passes through the decoder: once now, as a member of the list of elements, and also later, through its type-specific decoder (i.e., @racket[_string-proc], @racket[_symbol-proc], and so on).

@examples[#:eval my-eval
(define tx '(div "Double" "\n" "your" "\n" "pleasure")) 
(decode tx #:txexpr-elements-proc (λ(es) (map (λ(e)(format "~a~a" e e)) es)))
(decode tx #:txexpr-elements-proc (λ(es) (map (λ(e)(format "~a~a" e e)) es))
#:string-proc (λ(s) (string-upcase s)))
]

So why do you need @racket[_txexpr-elements-proc]? Because some types of element decoding depend on context, thus it's necessary to handle the elements as a group. For instance, paragraph detection. The behavior is not merely a @racket[map] across each element:

@examples[#:eval my-eval
(define (paras tx) (decode tx #:txexpr-elements-proc detect-paragraphs))
(paras '(body "The first paragraph." "\n\n")) 
(paras '(body "The first paragraph." "\n\n" "And another.")) 
(paras '(body "The first paragraph." "\n\n" "And another." "\n\n")) 
]


The @racket[_block-txexpr-proc] argument is a procedure that operates on tagged X-expressions that are deemed block-level (as opposed to inline) elements. That is, they meet the @racket[block-txexpr?] test. (See also @racket[register-block-tag].)





Finally, the @racket[_tags-to-exclude] argument is a list of tags that will be exempted from decoding. Though you could get the same result by testing the input within the individual decoding functions, that's tedious and potentially slower.

@examples[#:eval my-eval
(define tx '(p "I really think" (em "italics") "should be lowercase."))
(decode tx #:string-proc (λ(s) (string-upcase s)))
(decode tx #:string-proc (λ(s) (string-upcase s)) #:exclude-tags '(em))
]

The @racket[_tags-to-exclude] argument is useful if you're decoding source that's destined to become HTML. According to the HTML spec, material within a @racket[<style>] or @racket[<script>] block needs to be preserved literally. In this example, if the CSS and JavaScript blocks are capitalized, they won't work. So exclude @racket['(style script)], and problem solved.

@examples[#:eval my-eval
(define tx '(body (h1 [[class "Red"]] "Let's visit Planet Telex.") 
(style [[type "text/css"]] ".Red {color: green;}")
(script [[type "text/javascript"]] "var area = h * w;")))
(decode tx #:string-proc (λ(s) (string-upcase s)))
(decode tx #:string-proc (λ(s) (string-upcase s)) 
#:exclude-tags '(style script))
]

@section{Blocks}
@defmodule[pollen/decode/block]

Because it's convenient, Pollen categorizes tagged X-expressions into two categories: @italic{block} and @italic{inline}. Why is it convenient? When decoding, you often want to treat the two categories differently. Not that you have to. But this is how you can.

@defproc[
(register-block-tag
[tag txexpr-tag?])
void?]
Adds a tag to @racket[project-block-tags] so that @racket[block-txexpr?] will report it as a block, and @racket[decode] will process it with @racket[_block-txexpr-proc] rather than @racket[_inline-txexpr-proc].

@bold{Hey, this is important!} Pollen tries to do the right thing without being told. But this is the rare case where you have to be explicit. If you introduce a tag into your markup that you want treated as a block, you @bold{must} use this function to identify it, or you will get spooky behavior later on.

For instance, @racket[detect-paragraphs] knows that block elements in the markup shouldn't be wrapped in a @racket[p] tag. So if you introduce a new block element called @racket[bloq] without registering it as a block, misbehavior will follow:

@examples[#:eval my-eval
(define (paras tx) (decode tx #:txexpr-elements-proc detect-paragraphs))
(paras '(body "This wants to be a paragraph." "\n\n" (bloq "But not this."))) 
]

But once you register @racket[bloq] as a block, order is restored:

@examples[#:eval my-eval
(define (paras tx) (decode tx #:txexpr-elements-proc detect-paragraphs))
(register-block-tag 'bloq)
(paras '(body "This wants to be a paragraph." "\n\n" (bloq "But not this."))) 
]

If you find the idea of registering block tags unbearable, good news. The @racket[project-block-tags] include the standard HTML block tags by default. So if you just want to use things like @racket[div] and @racket[p] and @racket[h1–h6], you'll get the right behavior for free.

@examples[#:eval my-eval
(define (paras tx) (decode tx #:txexpr-elements-proc detect-paragraphs))
(paras '(body "This wants to be a paragraph." "\n\n" (div "But not this."))) 
]




@defproc[
(block-txexpr?
[v any/c])
boolean?]
Predicate that tests whether @racket[_v] is a tagged X-expression, and if so, whether the tag is among the @racket[project-block-tags]. If not, it is treated as inline.

@defparam[project-block-tags block-tags (listof txexpr-tag?)
          #:value html-block-tags]{
A parameter that defines the set of tags that @racket[decode] will treat as blocks. This parameter is initialized with the HTML block tags, namely:

@code[(format "~a" (dynamic-require 'css-tools/html 'block-tags))]}


