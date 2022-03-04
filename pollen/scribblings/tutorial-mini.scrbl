#lang scribble/manual

@(require scribble/eval (for-label pollen/unstable/pygments pollen/decode plot pollen/setup pollen/tag racket/base pollen/template txexpr racket/list racket/string))
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

I used @link["http://pygments.org/"]{Pygments} for syntax highlighting in @link["https://beautifulracket.com/"]{@italic{Beautiful Racket}}. Links to the source are available at the bottom of the article.

@itemlist[#:style 'ordered

@item{Make sure you have @code{pygments} already installed. @link["http://pygments.org/download/"]{Instructions here.} Pretty easy — for instance, on my Mac OS machine, it simply require @code{easy_install pygments} at the command line.}

@item{The @racketmodname[pollen/unstable/pygments] helper module provides a function called @racket[highlight]. To make @racket[highlight] available in your source file, you can either add the line @code{◊(require pollen/unstable/pygments)} to the source file itself, or put it in @racket["pollen.rkt"] and @racket[provide] it from there.}

@item{To invoke Pygments, use @racket[highlight] by providing a language name like @racket['python] in the brackets (note quote mark at front of name) and then the code you want highlighted between curly braces.

@codeblock{
#lang pollen
◊(require pollen/unstable/pygments)
◊highlight['python]{
for x in range(3):
    print x
}
}

When you run this file, you should see something like this, with the parsed syntax marked up into an X-expression:

@repl-output{
'(div ((class "highlight")) (table ((class "sourcetable")) (tbody (tr (td ((class "linenos")) (div ((class "linenodiv")) (pre "1\n2"))) (td ((class "code")) (div ((class "source")) (pre (span ((class "k")) "for") " " (span ((class "n")) "x") " " (span ((class "ow")) "in") " " (span ((class "nb")) "range") (span ((class "p")) "(") (span ((class "mi")) "3") (span ((class "p")) "):") "\n    " (span ((class "k")) "print") " " (span ((class "n")) "x") "\n")) "\n")))) "\n")
}
}

@item{To get highlighting, you'll still need to add styles to your CSS to change the appearance of the code. If you look at the @link["http://unitscale.com/mb/technique/styles.css.pp.html"]{CSS source in my demo article}, I just pasted a Pygments theme at the bottom (that @link["https://github.com/richleland/pygments-css"]{I picked up} and then edited with some variable values).
}

]
 
I concede that the last step isn’t convenient. But I haven’t yet figured out how it to make it easier. Larding the Pollen distribution with a bunch of Pygments themes doesn’t make sense. Moreover, even if convenient, they wouldn’t be editable / programmable, which is sort of the point of the whole exercise. 

Anyhow, that's why it's in the @code{unstable} category — it works, but I think it could be done better.

@subsection{Using Highlight.js with Pollen}

Because @link["https://highlightjs.org/"]{Highlight.js} is browser-based, it doesn’t require any high-level cooperation from Pollen. You just add it to your project like an image or webfont or other linked asset.


@itemlist[#:style 'ordered

@item{Download the @link["https://highlightjs.org/usage/"]{Highlight.js} library.}

@item{Add these lines to the @code{<head>} section of your @filepath{template.html} (or other template):

@terminal{
<link rel="stylesheet" href="/path/to/styles/default.css">
<script src="/path/to/highlight.js"></script>
<script>hljs.initHighlightingOnLoad();</script>
}
}

@item{With these resources loaded, Highlight.js will automatically highlight any code syntax with the markup @tt{<pre><code class="language-name">...</code></pre>}.  So in Pollen markup, you could write that directly like so: 

@codeblock{
#lang pollen/markup
◊pre{◊code[#:class "python"]{
for x in range(3):
    print x
}}}}

@item{Or if you wanted to match the notation for @racketmodname[pollen/unstable/pygments], you could write a @tt{highlight} function that expands to markup that works for Highlight.js:

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



@section{Math typesetting with MathJax}

@link["http://www.mathjax.org"]{MathJax} is a JavaScript library that implements the math-typesetting algorithms of TeX. It's easy to add MathJax to a Pollen project, and then invoke it with Pollen command notation in your source files.

@itemlist[#:style 'ordered

@item{Download the @link["http://docs.mathjax.org/en/latest/start.html"]{MathJax} library if you want to run the library locally, without a network connection. You can also use a MathJax CDN like @link["https://cdnjs.com/"]{cdnjs} and just link to the library across the network (I'll use this option in the example that follows).}

@item{Add these lines to the @code{<head>} section of your @filepath{template.html} (or other template). First, the MathJax library itself:

@terminal{
<script type="text/javascript"
  src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
</script>
}

Then, add any configuration options. For instance, this will activate the dollar sign as an inline-equation delimiter:

@terminal{
<script type="text/x-mathjax-config">
  MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$']]}});
</script>
}
}

@item{Make Pollen tag functions that add the delimiter characters and also a @tt{mathjax} wrapper tag that will trigger the JavaScript typesetting. For instance, suppose we wanted to denote inline equations with @code{◊${equation …}} and block equations with @code{◊$${equation …}}. Our tag functions could look like this:

@codeblock{
#lang pollen
◊(define ($ . xs)
  `(mathjax ,(apply string-append `("$" ,@"@"xs "$"))))
◊(define ($$ . xs)
  `(mathjax ,(apply string-append `("$$" ,@"@"xs "$$"))))
}
}
]


Putting it together, here's a minimal working example in two files (obviously in a larger project, you'd move those tag functions to a @filepath{pollen.rkt} file):

@fileblock["equation.html.pm"
@codeblock{
#lang pollen
◊(define ($ . xs)
  `(mathjax ,(apply string-append `("$" ,@"@"xs "$"))))
◊(define ($$ . xs)
  `(mathjax ,(apply string-append `("$$" ,@"@"xs "$$"))))

◊h1{I wonder if ◊${2^{\aleph_\alpha} = \aleph_{\alpha+1}}?}
}]



@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
<script type="text/javascript"
src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
</script>
<script type="text/x-mathjax-config">
MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$']]}});
</script>
</head>
<body>
◊(->html doc)
</body>
</html>
}]

By the way, there's no @code{pollen/math} module because this task doesn't seem complicated enough to merit it. What I just described is all you need.
