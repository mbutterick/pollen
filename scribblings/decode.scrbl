#lang scribble/manual

@(require scribble/eval pollen/decode pollen/world (prefix-in html: pollen/html) txexpr (for-label racket (except-in pollen #%module-begin) pollen/world pollen/cache pollen/decode txexpr xml pollen/html))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/decode xml racket/list txexpr))


@title{Decode}

@defmodule[pollen/decode]

The @racket[doc] export of a Pollen markup file is a simple X-expression. @italic{Decoding} refers to any post-processing of this X-expression. The @racket[pollen/decode] module provides tools for creating decoders.

The decode step can happen separately from the compilation of the file. But you can also attach a decoder to the markup file's @racket[root] node, so the decoding happens automatically when the markup is compiled, and thus automatically incorporated into @racket[doc]. (Following this approach, you could also attach multiple decoders to different tags within @racket[doc].)

You can, of course, embed function calls within Pollen markup. But since markup is optimized for authors, decoding is useful for operations that can or should be moved out of the authoring layer. 

One example is presentation and layout. For instance, @racket[detect-paragraphs] is a decoder function that lets authors mark paragraphs in their source simply by using two carriage returns. 

Another example is conversion of output into a particular data format. Most Pollen functions are optimized for HTML output, but one could write a decoder that targets another format.



@defproc[
(decode
[tagged-xexpr txexpr?]
[#:txexpr-tag-proc txexpr-tag-proc (txexpr-tag? . -> . txexpr-tag?) (λ(tag) tag)]
[#:txexpr-attrs-proc txexpr-attrs-proc (txexpr-attrs? . -> . txexpr-attrs?) (λ(attrs) attrs)]
[#:txexpr-elements-proc txexpr-elements-proc (txexpr-elements? . -> . txexpr-elements?) (λ(elements) elements)]
[#:block-txexpr-proc block-txexpr-proc (block-txexpr? . -> . xexpr?) (λ(tx) tx)]
[#:inline-txexpr-proc inline-txexpr-proc (txexpr? . -> . xexpr?) (λ(tx) tx)]
[#:string-proc string-proc (string? . -> . xexpr?) (λ(str) str)]
[#:symbol-proc symbol-proc (symbol? . -> . xexpr?) (λ(sym) sym)]
[#:valid-char-proc valid-char-proc (valid-char? . -> . xexpr?) (λ(vc) vc)]
[#:cdata-proc cdata-proc (cdata? . -> . xexpr?) (λ(cdata) cdata)]
[#:exclude-tags tags-to-exclude (listof symbol?) null]
)
txexpr?]
Recursively process a @racket[_tagged-xexpr], usually the one exported from a Pollen source file as @racket[doc]. 

This function doesn't do much on its own. Rather, it provides the hooks upon which harder-working functions can be hung. 

Recall from [future link: Pollen mechanics] that any tag can have a function attached to it. By default, the @racket[_tagged-xexpr] from a source file is tagged with @racket[root]. So the typical way to use @racket[decode] is to attach your decoding functions to it, and then define @racket[root] to invoke your @racket[decode] function. Then it will be automatically applied to every @racket[doc] during compile. 

For instance, here's how @racket[decode] is attached to @racket[root] in @italic{Butterick's Practical Typography}. There's not much to it —

@racketblock[
(define (root . items)
  (decode (make-txexpr 'root '() items)
          #:txexpr-elements-proc detect-paragraphs
          #:block-txexpr-proc (compose1 hyphenate wrap-hanging-quotes)
          #:string-proc (compose1 smart-quotes smart-dashes)
          #:exclude-tags '(style script)))
          ]

This illustrates another important point: even though @racket[decode] presents an imposing list of arguments, you're unlikely to use all of them at once. These represent possibilities, not requirements. For instance, let's see what happens when @racket[decode] is invoked without any of its optional arguments.

@examples[#:eval my-eval
(define tx '(root "I wonder" (em "why") "this works."))
(decode tx)
]

Right — nothing. That's because the default value for the decoding arguments is the identity function, @racket[(λ(x)x)]. So all the input gets passed through intact unless another action is specified.

The @racket[_*-proc] arguments of @racket[decode] take procedures that are applied to specific categories of elements within @racket[_txexpr].

The @racket[_txexpr-tag-proc] argument is a procedure that handles X-expression tags.

@examples[#:eval my-eval
(define tx '(p "I'm from a strange" (strong "namespace")))
(code:comment @#,t{Tags are symbols, so a tag-proc should return a symbol})
(decode tx #:txexpr-tag-proc (λ(t) (string->symbol (format "ns:~a" t))))
]

The @racket[_txexpr-attrs-proc] argument is a procedure that handles lists of X-expression attributes. (The @racketmodname[txexpr] module, included at no extra charge with Pollen, includes useful helper functions for dealing with these attribute lists.)

@examples[#:eval my-eval
(define tx '(p [[id "first"]] "If I only had a brain."))
(code:comment @#,t{Attrs is a list, so cons is OK for simple cases})
(decode tx #:txexpr-attrs-proc (λ(attrs) (cons '[class "PhD"] attrs )))
]

Note that @racket[_txexpr-attrs-proc] will change the attributes of every tagged X-expression, even those that don't have attributes. This is useful, because sometimes you want to add attributes where none existed before. But be careful, because the behavior may make your processing function overinclusive.

@examples[#:eval my-eval
(define tx '(div (p [[id "first"]] "If I only had a brain.") 
(p "Me too.")))
(code:comment @#,t{This will insert the new attribute everywhere})
(decode tx #:txexpr-attrs-proc (λ(attrs) (cons '[class "PhD"] attrs )))
(code:comment @#,t{This will add the new attribute only to non-null attribute lists})
(decode tx #:txexpr-attrs-proc 
(λ(attrs) (if (null? attrs) attrs (cons '[class "PhD"] attrs ))))
]


The @racket[_txexpr-elements-proc] argument is a procedure that operates on the list of elements that represents the content of each tagged X-expression. Note that each element of an X-expression is subject to two passes through the decoder: once now, as a member of the list of elements, and also later, through its type-specific decoder (i.e., @racket[_string-proc], @racket[_symbol-proc], and so on).

@examples[#:eval my-eval
(define tx '(div "Double" "\n" "toil" amp "trouble")) 
(code:comment @#,t{Every element gets doubled ...})
(decode tx #:txexpr-elements-proc (λ(es) (append-map (λ(e) `(,e ,e)) es)))
(code:comment @#,t{... but only strings get capitalized})
(decode tx #:txexpr-elements-proc (λ(es) (append-map (λ(e) `(,e ,e)) es))
#:string-proc (λ(s) (string-upcase s)))
]

So why do you need @racket[_txexpr-elements-proc]? Because some types of element decoding depend on context, thus it's necessary to handle the elements as a group. For instance, the doubling function above, though useless, requires handling the element list as a whole, because elements are being added.

A more useful example: paragraph detection. The behavior is not merely a @racket[map] across each element:

@examples[#:eval my-eval
(define (paras tx) (decode tx #:txexpr-elements-proc detect-paragraphs))
(code:comment @#,t{Context matters. Trailing whitespace is ignored ...})
(paras '(body "The first paragraph." "\n\n")) 
(code:comment @#,t{... but whitespace between strings is converted to a break.})
(paras '(body "The first paragraph." "\n\n" "And another.")) 
(code:comment @#,t{A combination of both types})
(paras '(body "The first paragraph." "\n\n" "And another." "\n\n")) 
]


The @racket[_block-txexpr-proc] argument and the @racket[_inline-txexpr-proc] arguments are procedures that operate on tagged X-expressions. If the X-expression meets the @racket[block-txexpr?] test, it is processed by @racket[_block-txexpr-proc]. Otherwise, it is processed by @racket[_inline-txexpr-proc]. Thus every tagged X-expression will be handled by one or the other. Of course, if you want block and inline elements to be handled the same way, you can set @racket[_block-txexpr-proc] and @racket[_inline-txexpr-proc] to be the same procedure.

@examples[#:eval my-eval
(define tx '(div "Please" (em "mind the gap") (h1 "Tuesdays only"))) 
(define add-ns (λ(tx) (make-txexpr 
    (string->symbol (format "ns:~a" (get-tag tx))) 
    (get-attrs tx) 
    (get-elements tx))))
(code:comment @#,t{div and h1 are block elements, so this will only affect them})
(decode tx #:block-txexpr-proc add-ns)
(code:comment @#,t{em is an inline element, so this will only affect it})
(decode tx #:inline-txexpr-proc add-ns)
(code:comment @#,t{this will affect all elements})
(decode tx #:block-txexpr-proc add-ns #:inline-txexpr-proc add-ns)
]

The @racket[_string-proc], @racket[_symbol-proc], @racket[_valid-char-proc], and @racket[_cdata-proc] arguments are procedures that operate on X-expressions that are strings, symbols, valid-chars, and CDATA, respectively. Deliberately, the output contracts for these procedures accept any kind of X-expression (meaning, the procedure can change the X-expression type).

@examples[#:eval my-eval
(code:comment @#,t{A div with string, entity, character, and cdata elements})
(define tx `(div "Moe" amp 62 ,(cdata #f #f "3 > 2;")))
(define rulify (λ(x) '(hr)))
(code:comment @#,t{The rulify function is selectively applied to each})
(print (decode tx #:string-proc rulify))
(print (decode tx #:symbol-proc rulify))
(print (decode tx #:valid-char-proc rulify))
(print (decode tx #:cdata-proc rulify))
] 




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

@section{Block}

Because it's convenient, Pollen categorizes tagged X-expressions into two categories: @italic{block} and @italic{inline}. Why is it convenient? When using @racket[decode], you often want to treat the two categories differently. Not that you have to. But this is how you can.

@defparam[project-block-tags block-tags (listof txexpr-tag?)]{
A parameter that defines the set of tags that @racket[decode] will treat as blocks. This parameter is initialized with the HTML block tags, namely:

@code[(format "~a" html:block-tags)]}


@defproc[
(register-block-tag
[tag txexpr-tag?])
void?]
Adds a tag to @racket[project-block-tags] so that @racket[block-txexpr?] will report it as a block, and @racket[decode] will process it with @racket[_block-txexpr-proc] rather than @racket[_inline-txexpr-proc].

Pollen tries to do the right thing without being told. But this is the rare case where you have to be explicit. If you introduce a tag into your markup that you want treated as a block, you @bold{must} use this function to identify it, or you will get spooky behavior later on.

For instance, @racket[detect-paragraphs] knows that block elements in the markup shouldn't be wrapped in a @racket[p] tag. So if you introduce a new block element called @racket[bloq] without registering it as a block, misbehavior will follow:

@examples[#:eval my-eval
(define (paras tx) (decode tx #:txexpr-elements-proc detect-paragraphs))
(paras '(body "I want to be a paragraph." "\n\n" (bloq "But not me."))) 
(code:comment @#,t{Wrong: bloq should not be wrapped})
]

But once you register @racket[bloq] as a block, order is restored:

@examples[#:eval my-eval
(define (paras tx) (decode tx #:txexpr-elements-proc detect-paragraphs))
(register-block-tag 'bloq)
(paras '(body "I want to be a paragraph." "\n\n" (bloq "But not me."))) 
(code:comment @#,t{Right: bloq is treated as a block})
]

If you find the idea of registering block tags unbearable, good news. The @racket[project-block-tags] include the standard HTML block tags by default. So if you just want to use things like @racket[div] and @racket[p] and @racket[h1–h6], you'll get the right behavior for free.

@examples[#:eval my-eval
(define (paras tx) (decode tx #:txexpr-elements-proc detect-paragraphs))
(paras '(body "I want to be a paragraph." "\n\n" (div "But not me."))) 
]


@defproc[
(block-txexpr?
[v any/c])
boolean?]
Predicate that tests whether @racket[_v] is a tagged X-expression, and if so, whether the tag is among the @racket[project-block-tags]. If not, it is treated as inline. To adjust how this test works, use @racket[register-block-tag].



@section{Typography}

An assortment of typography & layout functions, designed to be used with @racket[decode]. These aren't hard to write. So if you like these, use them. If not, make your own.


@defproc[
(whitespace?
[v any/c])
boolean?]
A predicate that returns @racket[#t] for any stringlike @racket[_v] that's entirely whitespace, but also the empty string, as well as lists and vectors that are made only of @racket[whitespace?] members. Following the @racket[regexp-match] convention, @racket[whitespace?] does not return @racket[#t] for a nonbreaking space. If you prefer that behavior, use @racket[whitespace/nbsp?]. 


@examples[#:eval my-eval
(whitespace? "\n\n   ")
(whitespace? (string->symbol "\n\n   "))
(whitespace? "")
(whitespace? '("" "  " "\n\n\n" " \n"))
(define nonbreaking-space (format "~a" #\u00A0))
(whitespace? nonbreaking-space)
]

@defproc[
(whitespace/nbsp?
[v any/c])
boolean?]
Like @racket[whitespace?], but also returns @racket[#t] for nonbreaking spaces.


@examples[#:eval my-eval
(whitespace/nbsp? "\n\n   ")
(whitespace/nbsp? (string->symbol "\n\n   "))
(whitespace/nbsp? "")
(whitespace/nbsp? '("" "  " "\n\n\n" " \n"))
(define nonbreaking-space (format "~a" #\u00A0))
(whitespace/nbsp? nonbreaking-space)
]


@defproc[
(smart-quotes
[str string?])
string?]
Convert straight quotes in @racket[_str] to curly according to American English conventions.

@examples[#:eval my-eval
(define tricky-string 
"\"Why,\" she could've asked, \"are we in O‘ahu watching 'Mame'?\"")
(display tricky-string)
(display (smart-quotes tricky-string))
]

@defproc[
(smart-dashes
[str string?])
string?]
In @racket[_str], convert three hyphens to an em dash, and two hyphens to an en dash, and remove surrounding spaces.

@examples[#:eval my-eval
(define tricky-string "I had a few --- OK, like 6--8 --- thin mints.")
(display tricky-string)
(display (smart-dashes tricky-string))
(code:comment @#,t{Monospaced font not great for showing dashes, but you get the idea})
]


@defproc[
(detect-linebreaks
[tagged-xexpr-elements txexpr-elements?]
[#:separator linebreak-sep string? world:linebreak-separator]
[#:insert linebreak xexpr? '(br)])
txexpr-elements?]
Within @racket[_tagged-xexpr-elements], convert occurrences of @racket[_linebreak-sep] (@racket["\n"] by default) to @racket[_linebreak], but only if @racket[_linebreak-sep] does not occur between blocks (see @racket[block-txexpr?]). Why? Because block-level elements automatically display on a new line, so adding @racket[_linebreak] would be superfluous. In that case, @racket[_linebreak-sep] just disappears.

@examples[#:eval my-eval
(detect-linebreaks '(div "Two items:" "\n" (em "Eggs") "\n" (em "Bacon")))
(detect-linebreaks '(div "Two items:" "\n" (div "Eggs") "\n" (div "Bacon")))
]

@defproc[
(detect-paragraphs
[elements txexpr-elements?]
[#:separator paragraph-sep string? world:paragraph-separator]
[#:tag paragraph-tag symbol? 'p]
[#:linebreak-proc linebreak-proc (txexpr-elements? . -> . txexpr-elements?) detect-linebreaks])
txexpr-elements?]
Find paragraphs within @racket[_elements], as denoted by @racket[_paragraph-sep], and wrap them with @racket[_paragraph-tag], unless the @racket[_element] is already a @racket[block-txexpr?] (because in that case, the wrapping is superfluous). Thus, as a consequence, if @racket[_paragraph-sep] occurs between two blocks, it's ignored. 

The @racket[_paragraph-tag] argument sets the tag used to wrap paragraphs. 

The @racket[_linebreak-proc] argument allows you to use a different linebreaking procedure other than the usual @racket[detect-linebreaks].

@examples[#:eval my-eval
(detect-paragraphs '("First para" "\n\n" "Second para"))
(detect-paragraphs '("First para" "\n\n" "Second para" "\n" "Second line"))
(detect-paragraphs '("First para" "\n\n" (div "Second block")))
(detect-paragraphs '((div "First block") "\n\n" (div "Second block")))
(detect-paragraphs '("First para" "\n\n" "Second para") #:tag 'ns:p)
(detect-paragraphs '("First para" "\n\n" "Second para" "\n" "Second line")
#:linebreak-proc (λ(x) (detect-linebreaks x #:insert '(newline))))
]

@defproc[
(wrap-hanging-quotes
[tx txexpr?]
[#:single-preprend single-preprender txexpr-tag? 'squo]
[#:double-preprend double-preprender txexpr-tag? 'dquo]
)
txexpr?]
Find single or double quote marks at the beginning of @racket[_tx] and wrap them in an X-expression with the tag @racket[_single-preprender] or @racket[_double-preprender], respectively. The default values are @racket['squo] and @racket['dquo].

@examples[#:eval my-eval
(wrap-hanging-quotes '(p "No quote to hang."))
(wrap-hanging-quotes '(p "“What? We need to hang quotes?”"))
]

In pro typography, quotation marks at the beginning of a line or paragraph are often shifted into the margin slightly to make them appear more optically aligned with the left edge of the text. With a reflowable layout model like HTML, you don't know where your line breaks will be. 

This function will simply insert the @racket['squo] and @racket['dquo] tags, which provide hooks that let you do the actual hanging via CSS, like so (actual measurement can be refined to taste):

@verbatim{squo {margin-left: -0.25em;}
dquo {margin-left: -0.50em;}
}

Be warned: there are many edge cases this function does not handle well.

@examples[#:eval my-eval
(code:comment @#,t{Argh: this edge case is not handled properly})
(wrap-hanging-quotes '(p "“" (em "What?") "We need to hang quotes?”"))
]

