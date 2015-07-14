#lang scribble/manual

@(require scribble/eval (for-label pollen/pygments pollen/decode plot pollen/world pollen/tag racket/base pollen/template txexpr racket/list racket/string))
@(require "mb-tools.rkt")

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/decode pollen/template pollen/tag xml racket/list txexpr))


@title[#:tag "mini-tutorial"]{Mini tutorials}

Smaller, self-contained tasks.

@section{Syntax highlighting}

Neither Pollen nor Racket has its own syntax-highlighting library, but you can use an external syntax highlighter. Two options: Pygments or Highlight.js.

Highlight.js is a JavaScript library, and thus meant to be run in the browser. So it's fine if your target is HTML, and you don't mind delegating that work to the browser. It's also easier to set up.

Pygments is a Python library (though you don't need to know any Python to use it with Pollen). It runs alongside Pollen (in a separate process) as pages are rendered, so the syntax highlighting can be baked into your compiled files. If you want to target other formats aside from HTML, this is the better choice. It requires a little more setup, but I doubt anyone who's made it this far will have trouble.

@subsection[#:tag "pygments-with-pollen"]{Using Pygments with Pollen}

I used @link["http://pygments.org/"]{Pygments} for syntax highlighting in @link["http://unitscale.com/mb/technique/dual-typed-untyped-library.html"]{this recent article made with Pollen}. Links to the source are available at the bottom of the article.

@itemlist[#:style 'ordered

@item{Make sure you have @code{pygments} already installed. @link["http://pygments.org/download/"]{Instructions here.} Pretty easy — for instance, on my OS X machine, I was able to get it done by doing @code{easy_install pygments} at the command line.}

@item{The @racketmodname[pollen/pygments] helper module provides a function called @racket[highlight]. To make @racket[highlight] available in your source file, you can either add the line @code{◊(require pollen/pygments)} to the source file itself, or put it in @racket["directory-require.rkt"] and @racket[provide] it from there.}

@item{To invoke Pygments, use @racket[highlight] by providing a language name like @racket['python] in the brackets (note quote mark at front of name) and then the code you want highlighted between curly braces.

@codeblock{
#lang pollen
◊(require pollen/pygments)
◊highlight['python]{
for x in range(3):
    print x
}
}

When you run this file, you should see something like this, with the parsed syntax marked up into an X-expression:

@repl-output{
'(div ((class "highlight")) (table ((class "sourcetable")) (tbody () (tr () (td ((class "linenos")) (div ((class "linenodiv")) (pre () "1\n2"))) (td ((class "code")) (div ((class "source")) (pre () (span ((class "k")) "for") " " (span ((class "n")) "x") " " (span ((class "ow")) "in") " " (span ((class "nb")) "range") (span ((class "p")) "(") (span ((class "mi")) "3") (span ((class "p")) "):") "\n    " (span ((class "k")) "print") " " (span ((class "n")) "x") "\n")) "\n")))) "\n")
}
}

@item{To get highlighting, you'll still need to add styles to your CSS to change the appearance of the code. If you look at the @link["http://unitscale.com/mb/technique/styles.css.pp.html"]{CSS source in my demo article}, I just pasted a Pygments theme at the bottom (that @link["https://github.com/richleland/pygments-css,"]{I picked up} and then edited with some variable values.)
}

]
 
I concede that the last step isn’t super convenient. But I haven’t yet figured out how it to make it easier. Larding the Pollen distribution with a bunch of Pygments themes doesn’t make sense. Moreover, even if convenient, they wouldn’t be editable / programmable, which is sort of the point of the whole exercise.

@subsection{Using Highlight.js with Pollen}

Because @link["https://highlightjs.org/"]{Highlight.js} is browser-based, it doesn’t require any high-level cooperation from Pollen. You just add it to your project like an image or webfont or other linked asset.


@itemlist[#:style 'ordered

@item{Download the @link["https://highlightjs.org/usage/"]{Highlight.js} library.}

@item{Add these lines to the @code{<head>} section of your @racket["template.html"] (or other template):

@repl-output{
<link rel="stylesheet" href="/path/to/styles/default.css">
<script src="/path/to/highlight.js"></script>
<script>hljs.initHighlightingOnLoad();</script>
}
}

@item{With these resources loaded, Highlight.js will automatically highlight any code syntax with the markup @tt{<pre><code class="language-name">...</code></pre>}.  So in Pollen markup, you could write that directly like so: 

@codeblock{
#lang pollen/markup
◊pre{◊code['class: "python"]{
for x in range(3):
    print x
}}}}

@item{Or if you wanted to match the notation for @racketmodname[pollen/pygments], you could write a @tt{highlight} function that expands to markup that works for Highlight.js:

@codeblock{
#lang pollen/markup
◊(define (highlight lang . xs)
   `(pre (code ((class ,(format "~a" lang))) ,@"@"xs))) 
◊highlight['python]{
  for x in range(3):
    print x
}}}
]

As above, I concede that it would be more convenient to have Pollen automatically drop the necessary incantations into the @tt{<head>} of your HTML. But as a matter of policy, I prefer to minimize magic behavior.
