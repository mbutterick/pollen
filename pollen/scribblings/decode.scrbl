#lang scribble/manual

@(require "mb-tools.rkt" scribble/eval pollen/decode pollen/setup txexpr racket/string (for-label racket(except-in pollen #%module-begin) pollen/setup pollen/cache pollen/decode txexpr xml))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/decode xml racket/list txexpr))


@title{Decode}

@defmodule[pollen/decode]

The @racket[doc] export of a Pollen markup file is a simple X-expression. @italic{Decoding} refers to any post-processing of this X-expression. The @racket[pollen/decode] module provides tools for creating decoders.

The decode step can happen separately from the compilation of the file. But you can also attach a decoder to the markup file's @racket[root] node, so the decoding happens automatically when the markup is compiled, and thus automatically incorporated into @racket[doc]. (Following this approach, you could also attach multiple decoders to different tags within @racket[doc].)

You can, of course, embed function calls within Pollen markup. But since markup is optimized for authors, decoding is useful for operations that can or should be moved out of the authoring layer. 

One example is presentation and layout. For instance, @racket[decode-paragraphs] is a decoder function that lets authors mark paragraphs in their source simply by using two carriage returns. 

Another example is conversion of output into a particular data format. Most Pollen functions are optimized for HTML output, but one could write a decoder that targets another format.



@defproc[
(decode
[tagged-xexpr txexpr?]
[#:txexpr-tag-proc txexpr-tag-proc (txexpr-tag? . -> . txexpr-tag?) (λ (tag) tag)]
[#:txexpr-attrs-proc txexpr-attrs-proc (txexpr-attrs? . -> . txexpr-attrs?) (λ (attrs) attrs)]
[#:txexpr-elements-proc txexpr-elements-proc (txexpr-elements? . -> . txexpr-elements?) (λ (elements) elements)]
[#:txexpr-proc txexpr-proc (txexpr? . -> . (or/c xexpr? (listof xexpr?))) (λ (tx) tx)]
[#:block-txexpr-proc block-txexpr-proc (block-txexpr? . -> . (or/c xexpr? (listof xexpr?))) (λ (tx) tx)]
[#:inline-txexpr-proc inline-txexpr-proc (txexpr? . -> . (or/c xexpr? (listof xexpr?))) (λ (tx) tx)]
[#:string-proc string-proc (string? . -> . (or/c xexpr? (listof xexpr?))) (λ (str) str)]
[#:entity-proc entity-proc ((or/c symbol? valid-char?) . -> . (or/c xexpr? (listof xexpr?))) (λ (ent) ent)]
[#:cdata-proc cdata-proc (cdata? . -> . (or/c xexpr? (listof xexpr?))) (λ (cdata) cdata)]
[#:exclude-tags tags-to-exclude (listof txexpr-tag?) null]
[#:exclude-attrs attrs-to-exclude txexpr-attrs? null]
)
(or/c xexpr/c (listof xexpr/c))]
Recursively process a @racket[_tagged-xexpr], usually the one exported from a Pollen source file as @racket[doc]. 

This function doesn't do much on its own. Rather, it provides the hooks upon which harder-working functions can be hung. 

Recall that in Pollen, all @secref["tags-are-functions"]. By default, the @racket[_tagged-xexpr] from a source file is tagged with @racket[root]. So the typical way to use @racket[decode] is to attach your decoding functions to it, and then define @racket[root] to invoke your @racket[decode] function. Then it will be automatically applied to every @racket[doc] during compile. 

For instance, here's how @racket[decode] is attached to @racket[root] in @link["http://practicaltypography.com"]{@italic{Butterick's Practical Typography}}. There's not much to it —

@racketblock[
(define (root . items)
  (decode (txexpr 'root '() items)
          #:txexpr-elements-proc decode-paragraphs
          #:block-txexpr-proc (compose1 hyphenate wrap-hanging-quotes)
          #:string-proc (compose1 smart-quotes smart-dashes)
          #:exclude-tags '(style script)))
          ]

@margin-note{The @racket[hyphenate] function is not part of Pollen, but rather the @link["http://github.com/mbutterick/hyphenate"]{@racket[hyphenate] package}, which you can install separately.}

This illustrates another important point: even though @racket[decode] presents an imposing list of arguments, you're unlikely to use all of them at once. These represent possibilities, not requirements. For instance, let's see what happens when @racket[decode] is invoked without any of its optional arguments.

@examples[#:eval my-eval
(define tx '(root "I wonder" (em "why") "this works."))
(decode tx)
]

Right — nothing. That's because the default value for the decoding arguments is the identity function, @racket[(λ (x)x)]. So all the input gets passed through intact unless another action is specified.

The @racket[_*-proc] arguments of @racket[decode] take procedures that are applied to specific categories of elements within @racket[_txexpr].

The @racket[_txexpr-tag-proc] argument is a procedure that handles X-expression tags.

@examples[#:eval my-eval
(define tx '(p "I'm from a strange" (strong "namespace")))
(code:comment @#,t{Tags are symbols, so a tag-proc should return a symbol})
(decode tx #:txexpr-tag-proc (λ (t) (string->symbol (format "ns:~a" t))))
]

The @racket[_txexpr-attrs-proc] argument is a procedure that handles lists of X-expression attributes. (The @racketmodname[txexpr] module, included at no extra charge with Pollen, includes useful helper functions for dealing with these attribute lists.)

@examples[#:eval my-eval
(define tx '(p ((id "first")) "If I only had a brain."))
(code:comment @#,t{Attrs is a list, so cons is OK for simple cases})
(decode tx #:txexpr-attrs-proc (λ (attrs) (cons '[class "PhD"] attrs )))
]

Note that @racket[_txexpr-attrs-proc] will change the attributes of every tagged X-expression, even those that don't have attributes. This is useful, because sometimes you want to add attributes where none existed before. But be careful, because the behavior may make your processing function overinclusive.

@examples[#:eval my-eval
(define tx '(div (p ((id "first")) "If I only had a brain.") 
(p "Me too.")))
(code:comment @#,t{This will insert the new attribute everywhere})
(decode tx #:txexpr-attrs-proc (λ (attrs) (cons '[class "PhD"] attrs )))
(code:comment @#,t{This will add the new attribute only to non-null attribute lists})
(decode tx #:txexpr-attrs-proc 
(λ (attrs) (if (null? attrs) attrs (cons '[class "PhD"] attrs ))))
]


The @racket[_txexpr-elements-proc] argument is a procedure that operates on the list of elements that represents the content of each tagged X-expression. Note that each element of an X-expression is subject to two passes through the decoder: once now, as a member of the list of elements, and also later, through its type-specific decoder (i.e., @racket[_string-proc], @racket[_entity-proc], and so on).

@examples[#:eval my-eval
(define tx '(div "Double" "\n" "toil" amp "trouble")) 
(code:comment @#,t{Every element gets doubled ...})
(decode tx #:txexpr-elements-proc (λ (es) (append-map (λ (e) (list e e)) es)))
(code:comment @#,t{... but only strings get capitalized})
(decode tx #:txexpr-elements-proc (λ (es) (append-map (λ (e) (list e e)) es))
#:string-proc (λ (s) (string-upcase s)))
]

So why do you need @racket[_txexpr-elements-proc]? Because some types of element decoding depend on context, thus it's necessary to handle the elements as a group. For instance, paragraph decoding. The behavior is not merely a @racket[map] across each element, because elements are being removed and altered contextually:

@examples[#:eval my-eval
(define (paras tx) (decode tx #:txexpr-elements-proc decode-paragraphs))
(code:comment @#,t{Context matters. Trailing whitespace is ignored ...})
(paras '(body "The first paragraph." "\n\n")) 
(code:comment @#,t{... but whitespace between strings is converted to a break.})
(paras '(body "The first paragraph." "\n\n" "And another.")) 
(code:comment @#,t{A combination of both types})
(paras '(body "The first paragraph." "\n\n" "And another." "\n\n")) 
]


The @racket[_txexpr-proc], @racket[_block-txexpr-proc], and @racket[_inline-txexpr-proc] arguments are procedures that operate on tagged X-expressions. If the X-expression meets the @racket[block-txexpr?] test, it's processed by @racket[_block-txexpr-proc]. Otherwise, it's inline, so it's processed by @racket[_inline-txexpr-proc]. (Careful, however — these aren't mutually exclusive, because @racket[_block-txexpr-proc] operates on all the elements of a block, including other tagged X-expressions within.) Then both categories are processed by @racket[_txexpr-proc]. 

@examples[#:eval my-eval
(define tx '(div "Please" (em "mind the gap") (h1 "Tuesdays only"))) 
(define add-ns (λ (tx) (txexpr 
    (string->symbol (format "ns:~a" (get-tag tx))) 
    (get-attrs tx) 
    (get-elements tx))))
(code:comment @#,t{div and h1 are block elements, so this will only affect them})
(decode tx #:block-txexpr-proc add-ns)
(code:comment @#,t{em is an inline element, so this will only affect it})
(decode tx #:inline-txexpr-proc add-ns)
(code:comment @#,t{this will affect all elements})
(decode tx #:block-txexpr-proc add-ns #:inline-txexpr-proc add-ns)
(code:comment @#,t{as will this})
(decode tx #:txexpr-proc add-ns)
]

The @racket[_string-proc], @racket[_entity-proc], and @racket[_cdata-proc] arguments are procedures that operate on X-expressions that are strings, entities, and CDATA, respectively. Deliberately, the output contracts for these procedures accept any kind of X-expression (meaning, the procedure can change the X-expression type).

@examples[#:eval my-eval
(code:comment @#,t{A div with string, entity, and cdata elements})
(define tx `(div "Moe" amp 62 ,(cdata #f #f "3 > 2;")))
(define rulify (λ (x) '(hr)))
(code:comment @#,t{The rulify function is selectively applied to each})
(print (decode tx #:string-proc rulify))
(print (decode tx #:entity-proc rulify))
(print (decode tx #:cdata-proc rulify))
] 

Note that entities come in two flavors — symbolic and numeric — and @racket[_entity-proc] affects both. If you only want to affect one or the other, you can add a test within @racket[_entity-proc]. Symbolic entities can be decodeed with @racket[symbol?], and numeric entities with @racket[valid-char?]:

@examples[#:eval my-eval
(define tx `(div amp 62))
(define symbolic-detonate (λ (x) (if (symbol? x) 'BOOM x)))
(print (decode tx #:entity-proc symbolic-detonate))
(define numeric-detonate (λ (x) (if (valid-char? x) 'BOOM x)))
(print (decode tx #:entity-proc numeric-detonate))
] 

The five previous procedures — @racket[_block-txexpr-proc], @racket[_inline-txexpr-proc], @racket[_string-proc], @racket[_entity-proc], and @racket[_cdata-proc] — can return either a single X-expression, or a list of X-expressions, which will be spliced into the parent at the same point.

For instance, earlier we saw how to double elements by using @racket[_txexpr-elements-proc]. But you can accomplish the same thing on a case-by-case basis by returning a list of values:

@examples[#:eval my-eval
(code:comment @#,t{A div with string, entity, and inline-txexpr elements})
(define tx `(div "Axl" amp (span "Slash")))
(define doubler (λ (x) (list x x)))
(code:comment @#,t{The doubler function is selectively applied to each type of element})
(print (decode tx #:string-proc doubler))
(print (decode tx #:entity-proc doubler))
(print (decode tx #:inline-txexpr-proc doubler))
] 

Caution: when returning list values, it's possible to trip over the unavoidable ambiguity between a @racket[txexpr?] and a list of @racket[xexpr?]s that happens to begin with a symbolic entity: 

@examples[#:eval my-eval
(code:comment @#,t{An ambiguous expression})
(define amb '(guitar "player-name"))
(and (txexpr-elements? amb) (txexpr? amb))
(code:comment @#,t{Ambiguity in context})
(define x '(gnr "Izzy" "Slash"))
(define rockit (λ (str) (list 'guitar str)))
(code:comment @#,t{Expecting '(gnr guitar "Izzy" guitar "Slash") from next line,
but return value will be treated as tagged X-expression})
(decode x #:string-proc rockit)
(code:comment @#,t{Changing the order makes it unambiguous})
(define rockit2 (λ (str) (list str 'guitar)))
(decode x #:string-proc rockit2)
] 

The @racket[_tags-to-exclude] argument is a list of tags that will be exempted from decoding. Though you could get the same result by testing the input within the individual decoding functions, that's tedious and potentially slower.

@examples[#:eval my-eval
(define tx '(p "I really think" (em "italics") "should be lowercase."))
(decode tx #:string-proc string-upcase)
(decode tx #:string-proc string-upcase #:exclude-tags '(em))
]

The @racket[_tags-to-exclude] argument is useful if you're decoding source that's destined to become HTML. According to the HTML spec, material within a @racket[<style>] or @racket[<script>] block needs to be preserved literally. In this example, if the CSS and JavaScript blocks are capitalized, they won't work. So exclude @racket['(style script)], and problem solved.

@examples[#:eval my-eval
(define tx '(body (h1 ((class "Red")) "Let's visit Planet Telex.") 
(style ((type "text/css")) ".Red {color: green;}")
(script ((type "text/javascript")) "var area = h * w;")))
(decode tx #:string-proc string-upcase)
(decode tx #:string-proc string-upcase #:exclude-tags '(style script))
]

Finally, the @racket[_attrs-to-exclude] argument works the same way as @racket[_tags-to-exclude], but instead of excluding an element based on its tag, it excludes based on whether the element has a matching attribute/value pair.

@examples[#:eval my-eval
(define tx '(p (span "No attrs") (span ((id "foo")) "One attr")))
(decode tx #:string-proc string-upcase)
(decode tx #:string-proc string-upcase #:exclude-attrs '((id "foo")))
]

@defproc[
(decode-elements
[elements txexpr-elements?]
[#:txexpr-tag-proc txexpr-tag-proc (txexpr-tag? . -> . txexpr-tag?) (λ (tag) tag)]
[#:txexpr-attrs-proc txexpr-attrs-proc (txexpr-attrs? . -> . txexpr-attrs?) (λ (attrs) attrs)]
[#:txexpr-elements-proc txexpr-elements-proc (txexpr-elements? . -> . txexpr-elements?) (λ (elements) elements)]
[#:txexpr-proc txexpr-proc (txexpr? . -> . (or/c xexpr? (listof xexpr?))) (λ (tx) tx)]
[#:block-txexpr-proc block-txexpr-proc (block-txexpr? . -> . (or/c xexpr? (listof xexpr?))) (λ (tx) tx)]
[#:inline-txexpr-proc inline-txexpr-proc (txexpr? . -> . (or/c xexpr? (listof xexpr?))) (λ (tx) tx)]
[#:string-proc string-proc (string? . -> . (or/c xexpr? (listof xexpr?))) (λ (str) str)]
[#:entity-proc entity-proc ((or/c symbol? valid-char?) . -> . (or/c xexpr? (listof xexpr?))) (λ (ent) ent)]
[#:cdata-proc cdata-proc (cdata? . -> . (or/c xexpr? (listof xexpr?))) (λ (cdata) cdata)]
[#:exclude-tags tags-to-exclude (listof txexpr-tag?) null]
[#:exclude-attrs attrs-to-exclude txexpr-attrs? null]
)
(or/c xexpr/c (listof xexpr/c))]
Identical to @racket[decode], but takes @racket[txexpr-elements?] as input rather than a whole tagged X-expression. A convenience variant for use inside tag functions.


@defproc[
(block-txexpr?
[v any/c])
boolean?]
Predicate that tests whether @racket[_v] has a tag that is among the @racket[setup:block-tags]. If not, it is treated as inline.

This predicate affects the behavior of other functions. For instance, @racket[decode-paragraphs] knows that block elements in the markup shouldn't be wrapped in a @racket[p] tag. So if you introduce a new block element called @racket[bloq] without configuring it as a block, misbehavior will follow:

@examples[#:eval my-eval
(define (paras tx) (decode tx #:txexpr-elements-proc decode-paragraphs))
(paras '(body "I want to be a paragraph." "\n\n" (bloq "But not me."))) 
(code:comment @#,t{Wrong: bloq should not be wrapped})
]

To change how this test works, use a @racket[setup] submodule as described in @secref["setup-overrides"]:

@racketblock[
(module setup racket/base
  (provide (all-defined-out))
  (require pollen/setup)
  (define block-tags (cons 'bloq default-block-tags)))]

After that change, the result will be:

@racketresultfont{'(body (p "I want to be a paragraph.") (bloq "But not me."))}

The default block tags are: 

@racketidfont{@(string-join (map symbol->string default-block-tags) " ")}


@defproc[
(merge-newlines
[elements (listof xexpr?)])
(listof xexpr?)]
Within @racket[_elements], merge sequential newline characters into a single element. The newline string is controlled by @racket[setup:newline], and defaults to @val[default-newline].

@examples[#:eval my-eval
(merge-newlines '(p "\n" "\n" "foo" "\n" "\n\n" "bar" 
  (em "\n" "\n" "\n")))]


@defproc[
(decode-linebreaks
[elements (listof xexpr?)]
[linebreaker (or/c #f xexpr? (xexpr? xexpr? . -> . (or/c #f xexpr?))) '(br)])
(listof xexpr?)]
Within @racket[_elements], convert occurrences of the linebreak separator to @racket[_linebreaker], but only if the separator does not occur between blocks (see @racket[block-txexpr?]). Why? Because block-level elements automatically display on a new line, so adding @racket[_linebreaker] would be superfluous. In that case, the linebreak separator just disappears.

The linebreak separator is controlled by @racket[setup:linebreak-separator], and defaults to @val[default-linebreak-separator].

The @racket[_linebreaker] argument can either be @racket[#f] (which will delete the linebreaks), an X-expression (which will replace the linebreaks), or a function that takes two X-expressions and returns one. This function will receive the previous and next elements, to make contextual substitution possible.

@examples[#:eval my-eval
(decode-linebreaks '(div "Two items:" "\n" (em "Eggs") "\n" (em "Bacon")))
(decode-linebreaks '(div "Two items:" "\n" (em "Eggs") "\n" (em "Bacon")) #f)
(decode-linebreaks '(div "Two items:" "\n" (div "Eggs") "\n" (div "Bacon")))
(decode-linebreaks '(div "Two items:" "\n" (em "Eggs") "\n" (em "Bacon"))
 (λ (prev next) (if (and (txexpr? prev) (member "Eggs" prev)) '(egg-br) '(br))))
]

@defproc[
(decode-paragraphs
[elements (listof xexpr?)]
[paragraph-wrapper (or/c txexpr-tag? ((listof xexpr?) . -> . txexpr?)) 'p]
[#:linebreak-proc linebreak-proc ((listof xexpr?) . -> . (listof xexpr?)) decode-linebreaks]
[#:force? force-paragraph? boolean? #f])
(listof xexpr?)]
Find paragraphs within @racket[_elements] and wrap them with @racket[_paragraph-wrapper]. Also handle linebreaks using @racket[decode-linebreaks].

What counts as a paragraph? Any @racket[_elements] that are either a) explicitly set apart with a paragraph separator, or b) adjacent to a @racket[block-txexpr?] (in which case the paragraph-ness is implied).

The paragraph separator is controlled by @racket[setup:paragraph-separator], and defaults to @val[default-paragraph-separator].

@examples[#:eval my-eval
(decode-paragraphs '("Explicit para" "\n\n" "Explicit para"))
(decode-paragraphs '("Explicit para" "\n\n" "Explicit para" "\n" "Explicit line"))
(decode-paragraphs '("Implied para" (div "Block") "Implied para"))
]

If @racket[_element] is already a block, it will not be wrapped as a paragraph (because in that case, the wrapping would be superfluous). Thus, as a consequence, if @racket[_paragraph-sep] occurs between two blocks, it will be ignored (as in the example below using two sequential @racket[div] blocks.) Likewise, @racket[_paragraph-sep] will also be ignored if it occurs between a block and a non-block (because a paragraph break is already implied).

@examples[#:eval my-eval
(code:comment @#,t{The explicit "\n\n" makes no difference in these cases})
(decode-paragraphs '((div "First block") "\n\n" (div "Second block")))
(decode-paragraphs '((div "First block") (div "Second block")))
(decode-paragraphs '("Para" "\n\n" (div "Block")))
(decode-paragraphs '("Para" (div "Block")))
]

The @racket[_paragraph-wrapper] argument can either be an X-expression, or a function that takes a list of elements and returns one tagged X-expressions. This function will receive the elements of the paragraph, to make contextual wrapping possible. 

@examples[#:eval my-eval
(decode-paragraphs '("First para" "\n\n" "Second para") 'ns:p)
(decode-paragraphs '("First para" "\n\n" "Second para") 
 (λ (elems) `(ns:p ,@elems "!?!")))
]

The @racket[_linebreak-proc] argument allows you to use a different linebreaking procedure other than the usual @racket[decode-linebreaks].

@examples[#:eval my-eval
(decode-paragraphs '("First para" "\n\n" "Second para" "\n" "Second line")
#:linebreak-proc (λ (x) (decode-linebreaks x '(newline))))
]

The @racket[#:force?] option will wrap a paragraph tag around @racket[_elements], even if no explicit or implicit paragraph breaks are found. The @racket[#:force?] option is useful for when you want to guarantee that you always get a list of blocks.

@examples[#:eval my-eval
(decode-paragraphs '("This" (span "will not be") "a paragraph"))
(decode-paragraphs '("But this" (span "will be") "a paragraph") #:force? #t)
]


