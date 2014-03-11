#lang scribble/manual

@(require scribble/eval pollen/decode pollen/world (for-label racket (except-in pollen #%module-begin) pollen/world pollen/cache pollen/decode txexpr xml pollen/predicates pollen/decode/block))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/decode pollen/decode/block xml))

@section{Typography}
@defmodule[pollen/decode/typography]

An assortment of typography & layout functions, designed to be used with @racket[decode]. These aren't hard to write. So if you like these, use them. If not, make your own.

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
]


@defproc[
(convert-linebreaks
[tagged-xexpr-elements txexpr-elements?]
[#:separator linebreak-sep string? world:linebreak-separator]
[#:linebreak linebreak xexpr? '(br)])
txexpr-elements?]
Within @racket[_tagged-xexpr-elements], convert occurrences of @racket[_linebreak-sep] (@racket["\n"] by default) to @racket[_linebreak], but only if @racket[_linebreak-sep] does not occur between blocks (see @racket[block-txexpr?]). Why? Because block-level elements automatically display on a new line, so adding @racket[_linebreak] would be superfluous. In that case, @racket[_linebreak-sep] just disappears.

@examples[#:eval my-eval
(convert-linebreaks '(div "Two items:" "\n" (em "Eggs") "\n" (em "Bacon")))
(convert-linebreaks '(div "Two items:" "\n" (div "Eggs") "\n" (div "Bacon")))
]

@defproc[
(whitespace?
[v any/c])
boolean?]
Returns @racket[#t] for any stringlike @racket[_v] that's entirely whitespace, but also the empty string, as well as lists and vectors that are made only of @racket[whitespace?] members.

@examples[#:eval my-eval
(whitespace? "\n\n   ")
(whitespace? (string->symbol "\n\n   "))
(whitespace? "")
(whitespace? '("" "  " "\n\n\n" " \n"))
]

@defproc[
(detect-paragraphs
[elements txexpr-elements?]
[#:tag paragraph-tag symbol? 'p]
[#:separator paragraph-sep string? world:paragraph-separator]
[#:linebreak-proc linebreak-proc procedure? convert-linebreaks])
txexpr-elements?]
Find paragraphs within @racket[_elements], as denoted by @racket[_paragraph-sep], and wrap them with @racket[_paragraph-tag], unless the @racket[_element] is already a @racket[block-txexpr?] (because in that case, the wrapping is superfluous). Thus, as a consequence, if @racket[_paragraph-sep] occurs between two blocks, it's ignored. 

The @racket[_paragraph-tag] argument sets the tag used to wrap paragraphs. 

The @racket[_linebreak-proc] argument allows you to use a different linebreaking procedure other than the usual @racket[convert-linebreaks].

@examples[#:eval my-eval
(detect-paragraphs '("First para" "\n\n" "Second para"))
(detect-paragraphs '("First para" "\n\n" "Second para" "\n" "Second line"))
(detect-paragraphs '("First para" "\n\n" (div "Second block")))
(detect-paragraphs '((div "First block") "\n\n" (div "Second block")))
(detect-paragraphs '("First para" "\n\n" "Second para") #:tag 'ns:p)
(detect-paragraphs '("First para" "\n\n" "Second para" "\n" "Second line")
#:linebreak-proc (λ(x) (convert-linebreaks x #:linebreak '(newline))))

]


@section{Decode}

@defmodule[pollen/decode]

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
Recursively process a @racket[_tagged-xexpr], usually the one exported from a Pollen source file as @racket['doc]. This function doesn't do much on its own. Rather, it provides the hooks upon which harder-working functions can be hung.

@margin-note{This is different from the Scribble approach, where the decoding logic is fixed for every document. In Pollen, you only get the decoding you ask for, and you can customize it to any degree.}

By default, the @racket[_tagged-xexpr] from a source file is tagged with @racket[root]. Recall from @secref{Pollen mechanics} that any tag can have a function attached to it. So the typical way to use @racket[decode] is to attach your decoding functions to it, and then define @racket[root] to invoke your @racket[decode] function. Then it will be automatically applied to every @racket['doc] during compile. 

For instance, here's how @racket[decode] is attached to @racket['root] in @italic{Butterick's Practical Typography}:

@codeblock|{
(define (root . items) 
    (decode (make-txexpr 'root null items)
        #:xexpr-elements-proc detect-paragraphs
        #:block-xexpr-proc 
            (λ(bx) (wrap-hanging-quotes (nonbreaking-last-space bx)))
        #:string-proc (compose1 smart-quotes smart-dashes)))}|

That's it. Which illustrates another important point: even though @racket[decode] presents an imposing list of arguments, you're unlikely to use all of them at once. These represent possibilities, not requirements. To that end, let's see what happens when @racket[decode] is invoked without any of its optional arguments.

@examples[#:eval my-eval
(define tx '(root "I wonder" (em "why") "this works."))
(decode tx)
]

Right — nothing. That's because the default value for the decoding arguments is the identity function, @racket[(λ(x)x)]. So all the input gets passed through intact unless another action is specified.

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


The @racket[_block-txexpr-proc] argument and the @racket[_inline-txexpr-proc] arguments are procedures that operate on tagged X-expressions. If the X-expression meets the @racket[block-txexpr?] test, it is processed by @racket[_block-txexpr-proc]. Otherwise, it is processed by @racket[_inline-txexpr-proc]. Thus every tagged X-expression will be handled by one or the other. Of course, if you want block and inline elements to be handled the same way, you can set @racket[_block-txexpr-proc] and @racket[_inline-txexpr-proc] to be the same procedure.

@examples[#:eval my-eval
(define tx '(div "Please" (em "mind the gap") (h1 "Tuesdays only"))) 
(define add-ns (λ(tx) (cons (string->symbol (format "ns:~a" (car tx))) 
(cdr tx))))
(decode tx #:block-txexpr-proc add-ns)
(decode tx #:inline-txexpr-proc add-ns)
(decode tx #:block-txexpr-proc add-ns #:inline-txexpr-proc add-ns)
]

The @racket[_string-proc], @racket[_symbol-proc], @racket[_valid-char-proc], and @racket[_cdata-proc] arguments are procedures that operate on X-expressions that are strings, symbols, valid-chars, and CDATA, respectively. Deliberately, the output contracts for these procedures accept any kind of X-expression (meaning, the procedure can change the X-expression type).

@examples[#:eval my-eval
(define tx `(div "Moe" amp 62 ,(cdata #f #f "3 > 2;")))
(define rulify (λ(x) '(hr)))
(decode tx #:string-proc rulify)
(decode tx #:symbol-proc rulify)
(decode tx #:valid-char-proc rulify)
(decode tx #:cdata-proc rulify)
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

@section{Blocks}
@defmodule[pollen/decode/block]

Because it's convenient, Pollen categorizes tagged X-expressions into two categories: @italic{block} and @italic{inline}. Why is it convenient? When using @racket[decode], you often want to treat the two categories differently. Not that you have to. But this is how you can.

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
Predicate that tests whether @racket[_v] is a tagged X-expression, and if so, whether the tag is among the @racket[project-block-tags]. If not, it is treated as inline. To adjust how this test works, use @racket[register-block-tag].

@defparam[project-block-tags block-tags (listof txexpr-tag?)
          #:value html-block-tags]{
A parameter that defines the set of tags that @racket[decode] will treat as blocks. This parameter is initialized with the HTML block tags, namely:

@code[(format "~a" (dynamic-require 'css-tools/html 'block-tags))]}


