#lang scribble/manual
@(require scribble/bnf scribble/eval "utils.rkt"
          (for-syntax racket/base)
          (for-label (only-in scribble/reader
                              use-at-readtable) pollen/world pollen/tag))

@(define read-eval (make-base-eval))
@(interaction-eval #:eval read-eval (require (for-syntax racket/base)))

@(define (at-exp-racket)
   @racket[#, @hash-lang[] #, @racketmodname[at-exp] #, @racketidfont{racket}])

@title[#:tag "reader"]{Pollen ◊ commands}

@italic{Parts of this section were adapted from Matthew Flatt and Eli Barzilay's excellent documentation for Racket's text-processing language, called Scribble◊.}

@section{The golden rule}

Pollen uses a special character — the @italic{lozenge}, which looks like this: ◊ — to mark commands  within a Pollen source file. So when you put a ◊ in your source, whatever comes next will be treated as a command. If you don't, it will just be interpreted as plain text.


@section{The lozenge glyph (◊)}

I chose the lozenge as the command marker because a) it appears in almost every font, b) it's barely used in ordinary typesetting, c) it's not used in any programming language that I know of, and d) its shape and color allow it to stand out easily in code without being distracting. 

Here's how you type it:

@bold{Mac}: option + shift + V
@(linebreak)
@bold{Windows}:
@(linebreak)
@bold{Ubuntu}:

Still, if you don't want to use the lozenge as your command marker, you can use something else. Set Pollen's @racket[world:command-marker] value to whatever character you want. 

@margin-note{Scribble uses the @"@" sign as a delimiter. It's not a bad choice if you only work with Racket files. But as you use Pollen to work on other kinds of text-based files that commonly contain @"@" signs — HTML pages especially — it gets cumbersome. So I changed it.}

But don't knock the lozenge till you try it. 

@;--------------------------------------------------------------------
@section{The two command modes: text mode & Racket mode}

Pollen commands can be entered in one of two modes: @italic{text mode} or @italic{Racket mode}. Both modes start with a lozenge (@litchar["◊"]):

@racketblock[
 @#,BNF-seq[@litchar["◊"] @nonterm{command name} @litchar{[} @nonterm{Racket arguments ...} @litchar{]} @litchar["{"] @nonterm{text argument} @litchar["}"]]
@#,BNF-seq[@litchar["◊"]
            @litchar{(} @nonterm{Racket expression} @litchar{)}]
]

@bold{Text-mode commands}

A text-mode command has the three possible parts after the @litchar["◊"]:

@itemlist[
@item{The @italic{command name} appears immediately after the @litchar["◊"]. Typically it's a short word.} 
@item{The @italic{Racket arguments} appear between square brackets. Pollen is partly an interface to the Racket programming language. These arguments have to be entered using Racket conventions — e.g., a @tt{string of text} needs to be put in quotes as a @code{"string of text"}. If you like programming, you'll end up using these frequently. If you don't, you won't.}
@item{The @italic{text argument} appears between braces (aka curly brackets). You can put any ordinary text here. Unlike with the Racket arguments, you don't put quotes around the text.}
]

Each of the three parts is optional. You can also nest commands within each other. However:

@itemlist[
@item{You can never have spaces between the three parts.}
@item{Whatever parts you use must always appear in the order above.}
]

Here are a few examples of correct text-mode commands:

@codeblock|{
  #lang pollen
  ◊variable-name
  ◊tag{Text inside the tag.}
  ◊tag['attr: "value"]{Text inside the tag}
  ◊get-customer-id["Brennan Huff"]
  ◊tag{His ID is ◊get-customer-id["Brennan Huff"].}
}|

And some incorrect examples:

@codeblock|{
  #lang pollen
  ◊tag {Text inside the tag.} ; space between first and second parts
  ◊tag[Text inside the tag] ; text argument needs to be within braces
  ◊tag{Text inside the tag}['attr: "value"] ; wrong order 
}|

The next section describes each of these parts in detail.

@bold{Racket-mode commands}

If you're familiar with Racket expressions, you can use the Racket-mode commands to embed them within Pollen source files. It's simple: any Racket expression can become a Pollen command by adding @litchar["◊"] to the front. So in Racket, this code:

@codeblock|{
  #lang racket
  (define song "Revolution")
  (format "~a #~a" song (* 3 3))
}|

Can be converted to Pollen like so: 

@codeblock|{
  #lang pollen
  ◊(define song "Revolution")
  ◊(format "~a #~a" song (* 3 3))
}|

And in DrRacket, they produce the same output:

@nested[#:style 'inset]{@racketresult["Revolution #9"]}

Beyond that, there's not much to say about Racket mode — any valid expression you can write in Racket will also be a valid Racket-mode Pollen command.

@bold{The relationship of text mode and Racket mode}

Even if you don't plan to write a lot of Racket-mode commands, you should be aware that under the hood, Pollen is converting all commands in text mode to Racket mode. So a text-mode command that looks like this:

@racketblock[
  ◊headline[#:size 'enormous]{Man Bites Dog!}
]

Is actually being turned into a Racket-mode command like this:

@racketblock[
  (headline #:size 'enormous "Man Bites Dog!")
]

Thus a text-mode command is just an alternate way of writing a Racket-mode command. (More broadly, all of Pollen is just an alternate way of using Racket.)

The corollary is that you can always write Pollen commands using whichever mode is more convenient or readable. For instance, the earlier example, written in the Racket mode:

@codeblock|{
  #lang pollen
  ◊(define song "Revolution")
  ◊(format "~a #~a" song (* 3 3))
}|

Can be rewritten using text mode:

@codeblock|{
  #lang pollen
  ◊define[song]{Revolution}
  ◊format["~a #~a" song (* 3 3)]
}|

And it will work the same way.


@;--------------------------------------------------------------------
@subsection{The command name}

In Pollen, you'll typically use the command name for one of four purposes:

@itemlist[
@item{To invoke a tag function.}
@item{To invoke another function.}
@item{To insert the value of a variable.}
@item{To insert a comment.}
]

@;--------------------------------------------------------------------
@subsubsection{Invoking tag functions}

By default, Pollen treats every command name as a @italic{tag function}. As the name implies, a tag function creates a tagged X-expression with the command name as the tag, and the text argument as the content.

@codeblock|{
  #lang pollen
  ◊strong{Fancy Sauce, $1} 
}|

@racketoutput{@literal{'(strong "Fancy Sauce, $1")}}

To streamline markup, Pollen doesn't restrict you to a certain set of tags, nor does it make you define your tag functions ahead of time. Just type a tag, and you can start using it.


@codeblock|{
  #lang pollen
  ◊utterlyridiculoustagname{Oh really?}
}|
@racketoutput{@literal{'(utterlyridiculoustagname "Oh really?")}}



The one restriction is that you can't invent names for tag functions that are already being used for other commands. For instance, @tt{map} is a name permanently reserved by the Racket function @racket[map]. It's also a rarely-used HTML tag. But gosh, you really want to use it. Problem is, if you invoke it directly, Pollen will think you mean the other @racket[map]: 


@codeblock|{
  #lang pollen
  ◊map{Fancy Sauce, $1} 
}|

@racketerror{map: arity mismatch;@(linebreak)
the expected number of arguments does not match the given number@(linebreak)
  given: 1@(linebreak)
  arguments...:@(linebreak)
    "Fancy Sauce, $1"}
   
What to do? Read on.

@;--------------------------------------------------------------------
@subsubsection{Invoking other functions}

Though every command name starts out as a tag function, it doesn't necessarily end there. You have two options for invoking other functions: defining your own , or invoking others from Racket.

@bold{Defining your own functions}

Use the @racket[define] command to create your own function for a command name. After that, when you use the command name, you'll get the new behavior. For instance, recall this example showing the default tag-function behavior:

@codeblock|{
  #lang pollen
  ◊strong{Fancy Sauce, $1} 
}|

@racketoutput{@literal{'(strong "Fancy Sauce, $1")}}

We can define @tt{strong} to do something else, like add to the text:

@codeblock|{
  #lang pollen
  ◊(define (strong text) `(strong ,(format "Hey! Listen up! ~a" text)))
  ◊strong{Fancy Sauce, $1} 
}|

@racketoutput{@literal{'(strong "Hey! Listen up! Fancy Sauce, $1")}}

The replacement function has to accept any arguments that might get passed along, but it doesn't have to do anything with them. For instance, this function definition won't work because @tt{strong} is going to get a text argument that it's not defined to handle:

@codeblock|{
  #lang pollen
  ◊(define (strong) '(fib "1 1 2 3 5 8 13 ..."))
  ◊strong{Fancy Sauce, $1} 
}|

@racketerror{strong: arity mismatch;@(linebreak)
the expected number of arguments does not match the given number@(linebreak)
  expected: 0@(linebreak)
  given: 1@(linebreak)
  arguments...:@(linebreak)
    "Fancy Sauce, $1"}

Whereas in this version, @tt{strong} accepts an argument called @tt{text}, but then ignores it:

@codeblock|{
  #lang pollen
  ◊(define (strong text) '(fib "1 1 2 3 5 8 13 ..."))
  ◊strong{Fancy Sauce, $1} 
}|

@racketoutput{@literal{'(fib "1 1 2 3 5 8 13 ...")}}


You can attach any behavior to a command name. As your project evolves, you can also update the behavior of a command name. In that way, Pollen commands become a set of hooks to which you can attach more elaborate processing.

@bold{Using Racket functions}

You aren't limited to your own commands. Any function from Racket or any of its libraries can be invoked directly by using it as a command name:

[example]

Combining these two ideas, you can also invoke Racket functions indirectly, by attaching them to your custom command names:

[example]



As mentioned above, some command names already have behavior associated with them. But you can use a custom function to work around this. For instance, suppose we want to use @tt{map} as a tag even though Racket is using it for its own function called @racket[map]. 

First, we invent a command name that doesn't conflict. Let's call it @code{my-map}. As you learned above, Pollen will treat a new command name as a tag function by default:

@codeblock|{
#lang pollen
◊my-map{How I would love this to be a map.}
}|

@racketoutput{@literal{'(my-map "How I would love this to be a map.")}}


But @code{my-map} is not the tag we want. We need to define @code{my-map} to be a tag function for @tt{map}, which we can do with the Pollen helper @racket[make-tag-function]:


@codeblock|{
#lang pollen
◊(require pollen/tag)
◊(define my-map (make-tag-function 'map))
◊my-map{How I would love this to be a map.}
}|

@racketoutput{@literal{'(map "How I would love this to be a map.")}}

Now we can use @tt{map} as a tag by invoking @tt{my-map} instead.


@;--------------------------------------------------------------------
@subsubsection{Inserting the value of a variable}

A Pollen command name usually refers to a function, but it can also refer to a @italic{variable}, which is a simple data value. Once you define the variable, you can insert it into your source by using the ◊ notation without any other arguments:

@codeblock|{
#lang pollen
◊(define foo "bar")
The value of foo is ◊foo
> The value of foo is bar
}|

Be careful — if you include arguments, even blank ones, Pollen will treat the command name as a function. This won't work, because a variable is not a function:

@margin-note{To understand what happens here, recall the relationship between Pollen's command modes. The text-mode command @code{◊foo[]} becomes the Racket-mode command @code{(foo)}, which after variable substitution becomes @code{("bar")}. If you try to evaluate @code{("bar")} in DrRacket, you'll get the same error.}


@codeblock|{
#lang pollen
◊(define foo "bar")
The value of foo is ◊foo[]
> application: not a procedure;
 expected a procedure that can be applied to arguments
  given: "bar"
  arguments...: [none]
}|


The reason we can simply drop @code{◊foo} into the text argument of another Pollen command is that the variable @code{foo} holds a text value (i.e., a string). When appropriate, Pollen will convert a variable to text in a sensible way. For instance, numbers are automatically converted:

@codeblock|{
#lang pollen
◊(define zam 42)
The value of zam is ◊zam
> The value of zam is 42
}|

But if @code{zam} is a @racket[list], the conversion will fail:

@codeblock|{
#lang pollen
◊(define zam (list 2 4 6))
The value of zam is ◊zam
> Pollen parser: can't convert '(2 4 6) to string
}|



One exception to know about. In the examples above, there's a word space between the variable and the other text. But suppose you need to insert a variable into text so that there's no space in between. The simple ◊ notation above won't work, because it won't be clear where the variable name ends and the text begins. 

For instance, this example fails because Pollen looks for a variable called @code{fooic} (which doesn't exist) rather than @code{foo} (which does):

@margin-note{The ``procedure'' in the error message refers to @code{fooic}, which by default is treated as a tag function.}

@codeblock|{
#lang pollen
◊(define foo "bar")
Hyper◊fooic chamber
> Pollen parser: can't convert #<procedure> to string
}|


In this situation, you can surround the variable name with vertical bars to explicitly indicate where the name ends. The bars are not treated as part of the name, nor are they included in the result.

@codeblock|{
#lang pollen
◊(define foo "bar")
Hyper◊|foo|ic chamber
> Hyperbaric chamber
}|



@subsubsection{lalala}



Besides being a Racket identifier, the @nonterm{cmd} part of an
@tech{◊-form} can have Racket punctuation prefixes, which will end up
wrapping the @italic{whole} expression.

@scribble-examples|==={
  @`',@foo{blah}
  @#`#'#,@foo{blah}
}===|

When writing Racket code, this means that @litchar|{@`',@foo{blah}}|
is exactly the same as @litchar|{`@',@foo{blah}}| and
@litchar|{`',@@foo{blah}}|, but unlike the latter two, the first
construct can appear in body texts with the same meaning, whereas the
other two would not work (see below).

After the optional punctuation prefix, the @nonterm{cmd} itself is not
limited to identifiers; it can be @italic{any} Racket expression.

@scribble-examples|==={
  @(lambda (x) x){blah}
  @`(unquote foo){blah}
}===|

In addition, the command can be omitted altogether, which will omit it
from the translation, resulting in an S-expression that usually
contains, say, just strings:

@scribble-examples|==={
  @{foo bar
    baz}
  @'{foo bar
     baz}
}===|

If the command part begins with a @litchar{;} (with no newline between
the @litchar["@"] and the @litchar{;}), then the construct is a
comment.  There are two comment forms, one for arbitrary-text and
possibly nested comments, and another one for line comments:

@racketblock[
@#,BNF-seq[@litchar["@;{"] @kleenestar{@nonterm{any}} @litchar["}"]]

@#,BNF-seq[@litchar["@;"] @kleenestar{@nonterm{anything-else-without-newline}}]
]

In the first form, the commented body must still parse correctly; see
the description of the body syntax below.  In the second form, all
text from the @litchar["@;"] to the end of the line @italic{and} all
following spaces (or tabs) are part of the comment (similar to
@litchar{%} comments in TeX).

@scribble-examples|==={
  @foo{bar @; comment
       baz@;
       blah}
}===|

Tip: if you use an editor in some Scheme mode without support for
@tech{◊-forms}, balanced comments can be confusing, since the open brace
looks commented out, and the closing one isn't.  In such cases it is
useful to ``comment'' out the closing brace too:

@verbatim[#:indent 2]|==={
  @;{
    ...
  ;}
}===|

so the editor does not treat the file as having unbalanced
parentheses.

If only the @nonterm{cmd} part of an @tech{◊-form} is specified, then the
result is the command part only, without an extra set of parenthesis.
This makes it suitable for Racket escapes in body texts.  (More on this
below, in the description of the body part.)

@scribble-examples|==={
  @foo{x @y z}
  @foo{x @(* y 2) z}
  @{@foo bar}
}===|

Finally, note that there are currently no special rules for using
@litchar["@"] in the command itself, which can lead to things like:

@scribble-examples|==={
  @@foo{bar}{baz}
}===|





Here is one example:

@scribble-examples|==={
  @foo{blah blah blah}
}===|

The example shows how an input syntax is read as Racket syntax, not
what it evaluates to. If you want to see the translation of an example
into S-expression form, add a quote in front of it in a
@at-exp-racket[] module. For example, running

@verbatim[#:indent 2]|{
  #lang at-exp racket
  '@foo{blah blah blah}
}|

in DrRacket prints the output

@nested[#:style 'inset]{@racketresult[(foo "blah blah blah")]}

while omitting the quote

@verbatim[#:indent 2]|{
  #lang at-exp racket
  @foo{blah blah blah}
}|

triggers a syntax error because @racket[foo] is not bound, and

@verbatim[#:indent 2]|{
  #lang at-exp racket
  (define (foo str) (printf "He wrote ~s.\n" str))
  @foo{blah blah blah}
}|

prints the output

@nested[#:style 'inset]{@racketoutput{He wrote "blah blah blah".}}

Here are more examples of @tech{◊-forms}:

@scribble-examples|==={
  @foo{blah "blah" (`blah'?)}
  @foo[1 2]{3 4}
  @foo[1 2 3 4]
  @foo[#:width 2]{blah blah}
  @foo{blah blah
       yada yada}
  @foo{
    blah blah
    yada yada
  }
}===|

As seen in the last example, multiple lines and the newlines that
separate them are parsed to multiple Racket strings.  More generally,
a @nonterm{text-body} is made of text, newlines, and nested
@tech{◊-forms}, where the syntax for @tech{◊-forms} is the same whether it's
in a @nonterm{text-body} context as in a Racket context.  A
@nonterm{text-body} that isn't an @tech{◊-form} is converted to a string
expression for its @nonterm{parsed-body}; newlines and following
indentations are converted to @racket["\n"] and all-space string
expressions.

@scribble-examples|==={
  @foo{bar @baz{3}
       blah}
  @foo{@b{@u[3] @u{4}}
       blah}
  @C{while (*(p++))
       *p = '\n';}
}===|

The command part of an @tech{◊-form} is optional as well. In that case,
the @tech{◊-form} is read as a list, which usually counts as a function
application, but it also useful when quoted with the usual Racket
@racket[quote]:

@scribble-examples|==={
  @{blah blah}
  @{blah @[3]}
  '@{foo
     bar
     baz}
}===|

Finally, we can also drop the datum and text parts, which leaves us with
only the command---which is read as is, not within a parenthesized
form.  This is not useful when reading Racket code, but it can be used
inside a text block to escape a Racket identifier.  A vertical bar
(@litchar{|}) can be used to delimit the escaped identifier when
needed.

@scribble-examples|==={
  @foo
  @{blah @foo blah}
  @{blah @foo: blah}
  @{blah @|foo|: blah}
}===|

Actually, the command part can be any Racket expression (that does not
start with @litchar["["], @litchar["{"], or @litchar["|"]), which is
particularly useful with such escapes since they can be used with any
expression.

@scribble-examples|==={
  @foo{(+ 1 2) -> @(+ 1 2)!}
  @foo{A @"string" escape}
}===|

Note that an escaped Racket string is merged with the surrounding text
as a special case.  This is useful if you want to use the special
characters in your string, but escaping braces are not necessary if
they are balanced.

@scribble-examples|==={
  @foo{eli@"@"barzilay.org}
  @foo{A @"{" begins a block}
  @C{while (*(p++)) {
       *p = '\n';
     }}
}===|

In some cases, a text contains many literal ◊s, which can be
cumbersome to quote individually.  For such case, braces have an
alternative syntax: A block of text can begin with a
``@litchar["|{"]'' and terminated accordingly with a
``@litchar["}|"]''.  Furthermore, any nested @tech{◊-forms} must begin
with a ``@litchar["|@"]''.

@scribble-examples|==={
  @foo|{bar}@{baz}|
  @foo|{bar |@x{X} baz}|
  @foo|{bar |@x|{@}| baz}|
}===|

In cases when even this is not convenient enough, punctuation
characters can be added between the @litchar{|} and the braces and the
◊ in nested forms.  (The punctuation is mirrored for parentheses
and @litchar{<>}s.)  With this extension, Pollen syntax can be used as a
``here string'' replacement.

@scribble-examples|==={
  @foo|--{bar}@|{baz}--|
  @foo|<<{bar}@|{baz}>>|
}===|

On the flip side of this is, how can an ◊ sign be used in Racket
code?  This is almost never an issue, because Racket strings and
characters are still read the same, and @litchar["@"] is set as a
non-terminating reader macro so it can be used in Racket identifiers
anywhere except in the first character of an identifier.  When
@litchar["@"] must appear as the first character of an identifier, you
must quote the identifier just like other non-standard characters in
normal S-expression syntax: with a backslash or with vertical bars.

@scribble-examples|==={
  (define \@email "foo@bar.com")
  (define |@atchar| #\@)
}===|

Note that spaces are not allowed before a @litchar{[} or a
@litchar["{"], or they will be part of the following text (or Racket
code).  (More on using braces in body texts below.)

@scribble-examples|==={
  @foo{bar @baz[2 3] {4 5}}
}===|

Finally, remember that the Pollen is just an alternate for
S-expressions. Identifiers still get their meaning, as in any
Racket code, through the lexical context in which they appear.
Specifically, when the above @tech{◊-form} appears in a Racket expression
context, the lexical environment must provide bindings for
@racket[foo] as a procedure or a macro; it can be defined, required,
or bound locally (with @racket[let], for example).

@; FIXME: unfortunate code duplication
@interaction[
(eval:alts
  (let* ([formatter (lambda (fmt)
          (lambda args (format fmt (apply string-append args))))]
         [bf (formatter "*~a*")]
         [it (formatter "/~a/")]
         [ul (formatter "_~a_")]
         [text string-append])
    #,(tt "@text{@it{Note}: @bf{This is @ul{not} a pipe}.}"))
  (let* ([formatter (lambda (fmt)
          (lambda args (format fmt (apply string-append args))))]
         [bf (formatter "*~a*")]
         [it (formatter "/~a/")]
         [ul (formatter "_~a_")]
         [text string-append])
    @text{@it{Note}: @bf{This is @ul{not} a pipe}.}))
]


@;--------------------------------------------------------------------
@section{The Datum Part}

The datum part can contains arbitrary Racket expressions, which
are simply stacked before the body text arguments:

@scribble-examples|==={
  @foo[1 (* 2 3)]{bar}
  @foo[@bar{...}]{blah}
}===|

The body part can still be omitted, which is essentially an
alternative syntax for plain (non-textual) S-expressions:

@scribble-examples|==={
  @foo[bar]
  @foo{bar @f[x] baz}
}===|

The datum part can be empty, which makes no difference, except when
the body is omitted.  It is more common, however, to use an empty body
for the same purpose.

@scribble-examples|==={
  @foo[]{bar}
  @foo[]
  @foo
  @foo{}
}===|

The most common use of the datum part is for Racket forms that expect
keyword-value arguments that precede the body of text arguments.

@scribble-examples|==={
  @foo[#:style 'big]{bar}
}===|

@;--------------------------------------------------------------------
@section{The Body Part}

The syntax of the body part is intended to be as convenient as
possible for free text.  It can contain almost any text---the only
characters with special meaning is @litchar["@"] for sub-@tech{◊-forms},
and @litchar["}"] for the end of the text.  In addition, a
@litchar["{"] is allowed as part of the text, and it makes the
matching @litchar["}"] be part of the text too---so balanced braces
are valid text.

@scribble-examples|==={
  @foo{f{o}o}
  @foo{{{}}{}}
}===|

As described above, the text turns to a sequence of string arguments
for the resulting form.  Spaces at the beginning and end of lines are
discarded, and newlines turn to individual @racket["\n"] strings
(i.e., they are not merged with other body parts); see also the
information about newlines and indentation below. Spaces are
@italic{not} discarded if they appear after the open @litchar["{"]
(before the closing @litchar["}"]) when there is also text that
follows (precedes) it; specifically, they are preserved in a
single-line body.

@scribble-examples|==={
  @foo{bar}
  @foo{ bar }
  @foo[1]{ bar }
}===|

If @litchar["@"] appears in a body, then it is interpreted as Racket
code, which means that the ◊-reader is applied recursively, and the
resulting syntax appears as part of the S-expression, among other
string contents.

@scribble-examples|==={
  @foo{a @bar{b} c}
}===|

If the nested ◊ construct has only a command---no body or datum
parts---it will not appear in a subform.  Given that the command part
can be any Racket expression, this makes ◊ a general escape to
arbitrary Racket code.

@scribble-examples|==={
  @foo{a @bar c}
  @foo{a @(bar 2) c}
}===|

This is particularly useful with strings, which can be used to include
arbitrary text.

@scribble-examples|==={
  @foo{A @"}" marks the end}
}===|

Note that the escaped string is (intentionally) merged with the rest
of the text.  This works for @litchar["@"] too:

@scribble-examples|==={
  @foo{The prefix: @"@".}
  @foo{@"@x{y}" --> (x "y")}
}===|

@;--------------------------------------------------------------------
@subsection[#:tag "alt-body-syntax"]{Alternative Body Syntax}

In addition to the above, there is an alternative syntax for the body,
one that specifies a new marker for its end: use @litchar["|{"] for
the opening marker to have the text terminated by a @litchar["}|"].

@scribble-examples|==={
  @foo|{...}|
  @foo|{"}" follows "{"}|
  @foo|{Nesting |{is}| ok}|
}===|

This applies to sub-@tech{◊-forms} too---the @litchar["@"] must be
prefixed with a @litchar{|}:

@scribble-examples|==={
  @foo|{Maze
        |@bar{is}
        Life!}|
  @t|{In |@i|{sub|@"@"s}| too}|
}===|

Note that the subform uses its own delimiters, @litchar{{...}} or
@litchar{|{...}|}.  This means that you can copy and paste Pollen
text with @tech{◊-forms} freely, just prefix the @litchar["@"] if the
immediate surrounding text has a prefix.

For even better control, you can add characters in the opening
delimiter, between the @litchar{|} and the @litchar["{"].
Characters that are put there (non alphanumeric ASCII characters only,
excluding @litchar["{"] and @litchar["@"]) should also be used for
sub-@tech{◊-forms}, and the end-of-body marker should have these characters
in reverse order with paren-like characters (@litchar{(},
@litchar{[}, @litchar{<}) mirrored.

@scribble-examples|==={
  @foo|<<<{@x{foo} |@{bar}|.}>>>|
  @foo|!!{X |!!@b{Y}...}!!|
}===|

Finally, remember that you can use an expression escape with a Racket
string for confusing situations.  This works well when you only need
to quote short pieces, and the above works well when you have larger
multi-line body texts.

@;--------------------------------------------------------------------
@subsection{Racket Expression Escapes}

In some cases, you may want to use a Racket identifier (or a number or
a boolean etc.) in a position that touches the following text; in
these situations you should surround the escaped Racket expression by
a pair of @litchar{|} characters.  The text inside the bars is
parsed as a Racket expression.

@scribble-examples|==={
  @foo{foo@bar.}
  @foo{foo@|bar|.}
  @foo{foo@3.}
  @foo{foo@|3|.}
}===|

This form is a generic Racket expression escape, there is no body text
or datum part when you use this form.

@scribble-examples|==={
  @foo{foo@|(f 1)|{bar}}
  @foo{foo@|bar|[1]{baz}}
}===|

This works for string expressions too, but note that unlike the above,
the string is (intentionally) not merged with the rest of the text:

@scribble-examples|==={
  @foo{x@"y"z}
  @foo{x@|"y"|z}
}===|

Expression escapes also work with @italic{any} number of expressions,

@scribble-examples|==={
  @foo{x@|1 (+ 2 3) 4|y}
  @foo{x@|*
          *|y}
}===|

It seems that @litchar["@||"] has no purpose---but remember that these escapes
are never merged with the surrounding text, which can be useful when
you want to control the sub expressions in the form.

@scribble-examples|==={
  @foo{Alice@||Bob@|
       |Carol}
}===|

Note that @litchar["@|{...}|"] can be parsed as either an escape expression or
as the Racket command part of an @tech{◊-form}.  The latter is used in this case
(since there is little point in Racket code that uses braces.

@scribble-examples|==={
  @|{blah}|
}===|

@;--------------------------------------------------------------------
@subsection{Comments}

As noted above, there are two kinds of Pollen comments: @litchar|{@;{...}}| is
a (nestable) comment for a whole body of text (following the same
rules for @tech{◊-forms}), and @litchar|{@;...}| is a line-comment.

@scribble-examples|==={
  @foo{First line@;{there is still a
                    newline here;}
       Second line}
}===|

One useful property of line-comments is that they continue to the end
of the line @italic{and} all following spaces (or tabs).  Using this,
you can get further control of the subforms.

@scribble-examples|==={
  @foo{A long @;
       single-@;
       string arg.}
}===|

Note how this is different from using @litchar["@||"]s in that strings
around it are not merged.

@;--------------------------------------------------------------------
@subsection{Spaces, Newlines, and Indentation}

The Pollen syntax treats spaces and newlines in a special way is
meant to be sensible for dealing with text.  As mentioned above,
spaces at the beginning and end of body lines are discarded, except
for spaces between a @litchar["{"] and text, or between text and a
@litchar["}"].

@scribble-examples|==={
  @foo{bar}
  @foo{ bar }
  @foo{ bar
       baz }
}===|

A single newline that follows an open brace or precedes a closing
brace is discarded, unless there are only newlines in the body; other
newlines are read as a @racket["\n"] string

@scribble-examples|==={
  @foo{bar
  }
  @foo{
    bar
  }
  @foo{

    bar

  }
  @foo{
    bar

    baz
  }
  @foo{
  }
  @foo{

  }
  @foo{ bar
       baz }
}===|

Spaces at the beginning of body lines do not appear in the resulting
S-expressions, but the column of each line is noticed, and all-space
indentation strings are added so the result has the same indentation.
A indentation string is added to each line according to its distance
from the leftmost syntax object (except for empty lines).  (Note: if
you try these examples on a Racket REPL, you should be aware that
the reader does not know about the ``@litchar{> }'' prompt.)

@scribble-examples|==={
  @foo{
    bar
    baz
    blah
  }
  @foo{
    begin
      x++;
    end}
  @foo{
      a
     b
    c}
}===|

If the first string came from the opening @litchar["{"] line, it is
not prepended with an indentation (but it can affect the leftmost
syntax object used for indentation).  This makes sense when formatting
structured code as well as text (see the last example in the following
block).

@scribble-examples|==={
  @foo{bar
         baz
       bbb}
  @foo{ bar
          baz
        bbb}
  @foo{bar
     baz
     bbb}
  @foo{ bar
     baz
     bbb}
  @foo{ bar
     baz
       bbb}
  @text{Some @b{bold
    text}, and
    more text.}
}===|

Note that each ◊-form is parsed to an S-expression that has its own
indentation.  This means that Pollen source can be indented like
code, but if indentation matters then you may need to apply
indentation of the outer item to all lines of the inner one.  For
example, in

@litchar/lines|==={
  @code{
    begin
      i = 1, r = 1
      @bold{while i < n do
              r *= i++
            done}
    end
  }
}===|

a formatter will need to apply the 2-space indentation to the
rendering of the @racket[bold] body.

Note that to get a first-line text to be counted as a leftmost line,
line and column accounting should be on for the input port
(@racket[use-at-readtable] turns them on for the current input port).
Without this,

@litchar/lines|==={
  @foo{x1
         x2
         x3}
}===|

will not have 2-space indentations in the parsed S-expression if
source accounting is not on, but

@litchar/lines|==={
  @foo{x1
         x2
       x3}
}===|

will (due to the last line).  Pay attention to this, as it can be a
problem with Racket code, for example:

@litchar/lines|==={
  @code{(define (foo x)
          (+ x 1))}
}===|

For rare situations where spaces at the beginning (or end) of lines
matter, you can begin (or end) a line with a @litchar["@||"].

@scribble-examples|==={
  @foo{
    @|| bar @||
    @|| baz}
}===|

@; --------------------------------------------------
@(close-eval read-eval)

