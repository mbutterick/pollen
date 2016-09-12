#lang scribble/manual
@(require scribble/eval (for-label pollen/unstable/pygments pollen/decode plot pollen/setup pollen/tag racket/base pollen/template txexpr racket/list racket/string))
@(require "mb-tools.rkt")

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/decode pollen/template pollen/tag xml racket/list txexpr))


@title[#:tag "programming-pollen"]{Programming Pollen}

Core techniques for getting Pollen to do your bidding. 

I'll assume you're familiar with the workings of Racket generally and Pollen in particular, especially @secref["pollen-command-syntax"]. 

Experienced programmers, you will find this an easy read. 

Inexperienced programmers, you might find some parts a trudge, but keep going. Learning the material on this page will pay hefty dividends for as long as you use Pollen.

@section{Tag functions}

The @seclink["third-tutorial"]{third tutorial} introduced you to the idea that @secref["tags-are-functions"]. Let's recap the golden rules of tag functions that were first covered there:

@itemlist[#:style 'ordered

@item{Every Pollen tag calls a function with the same name.}

@item{The input values for that function are the attributes and elements of the tag.}

@item{The whole tag — tag name, attributes, and elements — is replaced with the return value of the called function.}
]

@subsection{Tag-function syntax}

To be safe, let's review what we mean by @italic{tag}, @italic{attributes}, and @italic{elements}, in Pollen-mode syntax:

@codeblock[#:keep-lang-line? #f]{
#lang pollen 
A tag alone: ◊get-author-name[]

A tag with elements: Adding ◊em{emphasis to words}.

A tag with attributes and elements: 
◊div['((attr1 "val1")(attr2 "val2"))]{My nice div.}

A tag with attributes and elements (alternate syntax):
◊div[#:attr1 "val1" #:attr2 "val2"]{My nice div.}
}

Let's also recall that these commands can be written in Racket mode equivalently:

@codeblock[#:keep-lang-line? #f]{
#lang pollen 
A tag alone: ◊(get-author-name)

A tag with elements: Adding ◊(em "emphasis to words").

A tag with attributes and elements: 
◊(div '((attr1 "val1")(attr2 "val2")) "My nice div.")
}

Let's also remember that a tag without attributes or elements is interpreted as a value. If that's what you want — for instance, when you @racket[define] a tag to hold a value — then great. But if you @racket[define] a tag as a function, you need to add square brackets to signal that you want to evaluate the function:

@codeblock[#:keep-lang-line? #f]{
#lang pollen 
◊(define author-name "Brennan Huff") ; a tag holding a value
◊(define (get-author-name) "Dale Doback") ; a tag function returning a value

To refer to the value held by a tag name:  
◊author-name or ◊|author-name|

If a tag name represents a function, you need to add square brackets 
to invoke the function: ◊get-author-name[]

This refers to the function as a value (not usually what you want): 
◊get-author-name}

@subsection{Point of no @code{return}} If you've written functions in other programming languages, you might be accustomed to using a @code{return} statement to send a value back from the function. This doesn't exist in Pollen or Racket — the return value of any function is just the last expression evaluated. In the example below, @code{"BAP"} becomes the return value because it's in the last position, and @code{"BOOM"} is ignored:

@fileblock["article.html.pm" @codeblock{
#lang pollen

◊(define (strong word) "BOOM" "BAP")

I want to attend ◊em{RacketCon ◊strong{this} year}.}]

Yielding:

@repl-output{'(root "I want to attend " (em "RacketCon " "BAP" " year") ".")}

@subsection{Multiple input values & rest arguments} Sometimes a tag will have only one word or string that becomes its input. More likely, however, it will have multiple values (this is inevitable with nested tags, because the results aren't concatenated). For instance, if we attach our function to @racket[em] rather than @racket[strong]:

@fileblock["article.html.pm" @codeblock{
#lang pollen

◊(define (em word) "BOOM")

I want to attend ◊em{RacketCon ◊strong{this} year}.}]

Look what happens:

@errorblock{
em: arity mismatch;
the expected number of arguments does not match the given number
expected: 1
  given: 3
}

The error arises because the @racket[em] function is getting three arguments — @code{"RacketCon " '(strong "this") " year"} — but has been defined to only accept one argument, @racket[word]. This is the ``arity mismatch.''

To fix this, it's better to get in the habit of writing tag functions that accept an indefinite number of input values. You do this by defining your function with a @italic{@seclink["contracts-rest-args" #:doc '(lib "scribblings/guide/guide.scrbl")]{rest argument}} (as in, ``give me the rest of the input values.'') To use a rest argument, put it last in your list of input arguments, and add a period @litchar{.} before:

@fileblock["article.html.pm" @codeblock{
#lang pollen

◊(define (em . parts) "BOOM")

I want to attend ◊em{RacketCon ◊strong{this} year}.}]

This time, the source file will run without an error, producing this:

@repl-output{'(root "I want to attend " "BOOM" ".")}

A rest argument like @racket[parts] is a @racket[list] of individual arguments. So if you want to unpack & process these arguments separately, you can use Racket's extensive list-processing functions (see @secref["pairs" #:doc '(lib "scribblings/guide/guide.scrbl")]). Also see @racket[quasiquote] below.

@subsection{Returning an X-expression} Often, you won't use a tag function to replace a whole tag with a string — you'll replace it with a different tag, described by an X-expression, like so:

@fileblock["article.html.pm" @codeblock{
#lang pollen

◊(define (em . parts) '(big "BOOM"))

I want to attend ◊em{RacketCon ◊strong{this} year}.}]

Which produces:

@repl-output{'(root "I want to attend " (big "BOOM") ".")}

The @racket[quote] mark @litchar{'} before the X-expression signals to Racket that you want to use what follows as a literal value.

To build X-expressions that are more elaborate, you have two options.

First is @racket[quasiquote]. Quasiquote works like quote, but starts with a backtick character @litchar{`}. What makes it ``quasi'' is that you can insert variables using the @racket[unquote] operator, which is a comma @litchar{,} or merge a list of values with the @racket[unquote-splicing] operator, which is a comma followed by an @"@" sign @litchar{,@"@"}.

Let's adapt the example above to use @racket[quasiquote]. Suppose we want to take the @racket[parts] we get as input and put them inside a @racket[big] tag. This is easy to notate with @racket[quasiquote] and the @racket[unquote-splicing] operator, because @racket[parts] is a list:

@fileblock["article.html.pm" @codeblock{
#lang pollen

◊(define (em . parts) `(big ,@"@"parts))

I want to attend ◊em{RacketCon ◊strong{this} year}.}]

Which produces this:

@repl-output{'(root "I want to attend " (big "RacketCon " (strong "this") " year") ".")}

Of course you can also nest X-expressions in your return value:

@fileblock["article.html.pm" @codeblock{
#lang pollen

◊(define (em . parts) `(extra (big ,@"@"parts)))

I want to attend ◊em{RacketCon ◊strong{this} year}.}]

The second option for building X-expressions is to use the @other-doc['(lib "txexpr/scribblings/txexpr.scrbl")] library that's included with Pollen (see those docs for more information).

@subsection{Using variables within strings} The usual way is to use the @racket[format] function:

@racket[(format "String with variable: ~a" variable-name)]

See the docs for @racket[format] and @racket[fprintf] for your options.

Be careful if you're working with integers and X-expressions — a raw integer is treated as a character code, not an integer string. Using @racket[format] is essential:

@examples[#:eval my-eval
(->html '(div "A raw integer indicates a character code: " 42))
(->html `(div "Use format to make it a string: " ,(format "~a" 42)))
]

@subsection{Parsing attributes}

Detecting attributes in an argument list can be tricky because a) the tag may or may not have attributes, b) those attributes may be in standard or abbreviated syntax. For this reason, Pollen provides a @racket[define-tag-function] macro (in the @racket[pollen/tag] library) that you can use in custom tag functions to separate the attributes and elements:

@fileblock["article.html.pm" @codeblock{
#lang pollen

◊(require pollen/tag)

◊(define-tag-function (em attributes elements) 
  `(extra ,attributes (big ,@"@"elements)))

I want to attend ◊em[#:key "value"]{RacketCon}.}]

This will move the @racket[elements] inside the @racket[big] tag, and attach the @racket[attributes] to the @racket[extra] tag:

@repl-output{'(root "I want to attend " (extra ((key "value")) (big "RacketCon")) ".")}
