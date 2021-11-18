#lang scribble/manual

@(require (for-label pollen/render racket/base pollen/core txexpr pollen/setup pollen/template pollen/pagetree sugar))

@(require "mb-tools.rkt")

@title[#:tag "second-tutorial"]{Second tutorial: Markdown, templates, & pagetrees}

In this tutorial, you'll use Pollen to publish a multiple-page article written in Markdown. You'll learn about:

@itemlist[

@item{Using Markdown files with the preprocessor}

@item{X-expressions}

@item{Markdown authoring mode}

@item{Templates}

@item{Pagetrees}

]

If you want the shortest possible introduction to Pollen, try the @secref["quick-tour"].

@section[#:tag-prefix "tutorial-2"]{Prerequisites}

I'll assume you've completed the @seclink["first-tutorial"]{first tutorial} and you understand how to create source files in DrRacket and view them in the project server. I won't be spelling out those tasks as I did before.

@section[#:tag "the-case-against-markdown"]{Optional reading: the case against Markdown}

I recognize that people like Markdown. I want people to like Pollen too, so that's why Pollen supports Markdown. 

But just to be clear about my own views:

I'm mystified by the popularity of Markdown among writers. I can agree that it's a clever and readable way of notating basic HTML. And sure, that makes it great for things like web comments, where speed and simplicity are primary virtues.

In longer-form writing, however, its shortcomings become evident. Like programming languages, the best writing tools maximize expressive possibilities, and minimize constraints. But Markdown is hugely constrained. First and worst, Markdown isn't semantic. It only knows about formatting, and in that regard, isn't that much of an improvement on tools like Microsoft Word. Second, even as a formatting-notation tool, it's limited to a small subset of the already-small set of formatting tags permitted in HTML. Third, it can't be extended by an author.

An animating principle of Pollen, as explained in the @secref["Backstory"], is that after 20 years, we ought to move beyond thinking of HTML as a source format. Since Markdown is just well-disguised HTML, a vote for Markdown is really a vote to continue the status quo (albeit with fewer angle brackets). For me, that's not good enough. I'm ready for the tools to expand to fit my ideas. I refuse to keep cutting down my ideas to fit the tools.

All that said, if you genuinely prefer Markdown, I'm not looking to pry it from your fingers. Pollen has great Markdown support (due entirely to Greg Hendershott's excellent @link["https://github.com/greghendershott/markdown/"]{Markdown parser} for Racket). It makes Markdown more useful. 

But let's make a deal, Markdown fans. Having met you more than halfway, will you at least consider that @seclink["pollen-vs-xml"]{Pollen markup} might be a better option for you than Markdown? Because it can notate anything that's in your brain, not just a subset of HTML? And if @secref["the-book-is-a-program"], the source for that book should look more like your brain, and less like HTML (or XML or LaTeX or ...)?

That's all I ask.


@section{Markdown in Pollen: two options}

There are two ways to use Markdown within Pollen:

@itemlist[#:style 'ordered
@item{Send Markdown files through the preprocessor.}
@item{Use Markdown authoring mode.}]

The preprocessor approach is better if you want to end up with a set of Markdown files that can be passed along to a HTML converter (or other Markdown-to-______ converter) downstream.

The authoring-mode approach is better if you want to end up with something other than Markdown, e.g., finished HTML files.

But we'll look at both.

@subsection{Using Markdown with the preprocessor}

Because Markdown is a text-based format, you can use the Pollen preprocessor to add programmatic features to existing Markdown files. (See @secref["Working_with_the_preprocessor"] in the @seclink["first-tutorial"]{first tutorial} if you need a refresher.)

Suppose we have a Markdown file called @filepath{brennan.md} that we want to use with the preprocessor. Create this file in DrRacket, save it, and start the project server in that directory.

@fileblock["brennan.md" 
@codeblock[#:keep-lang-line? #f]{
#lang pollen
My name is _Brennan_, and I enjoy:

+ boring sauce

+ 24 fish nuggets
}]

You'll be able to see this file in the project server, but for now, it's just a static file. Pollen isn't doing anything to it.

Let's change that. Consistent with the usual preprocessor practice, add @tt{#lang pollen} as the first line, and append the @filepath{.pp} file extension, so our new preprocessor-ready file looks like this:

@fileblock["brennan.md.pp" 
@codeblock{
#lang pollen

My name is _Brennan_, and I enjoy:

+ boring sauce

+ 24 fish nuggets
}]

Go back to the project server and you'll see the new filename. When you click on it, Pollen will render a new @filepath{brennan.md} file, but it will look the same as the one you had before. 

Now we'll change some of the values using Pollen commands:

@fileblock["brennan.md.pp" 
@codeblock{
#lang pollen

◊(define sauce-type "fancy")
◊(define nugget-type "chicken")
◊(define nugget-quantity (* 2 2 3))

My name is _Brennan_, and I enjoy:

+ ◊sauce-type sauce

+ ◊nugget-quantity ◊nugget-type nuggets
}]

When you reload this file in the project server, @filepath{brennan.md} will be regenerated, and will now look like this:

@terminal{
My name is _Brennan_, and I enjoy:

+ fancy sauce

+ 12 chicken nuggets}



Instead of running Markdown files through the preprocessor, you can also use Markdown authoring mode within Pollen. This is the better choice if you want to end up with rendered HTML files. 

But first, let's pause to clarify the general concept of an authoring mode.


@subsection{Authoring mode}

Though the preprocessor is useful, it limits you to inserting chunks of text at various positions into an existing file.

Pollen's @defterm{authoring mode}, by contrast, parses the whole source file into a special data structure called an @defterm{X-expression}. You can then process the whole X-expression any way you like, and output to any format you like — or multiple formats — using a @defterm{template}.

Compared to the preprocessor, authoring mode offers more abstraction and flexibility. Of course, it also requires a bit more effort to set up.

Pollen offers two authoring modes: one that uses Markdown syntax (which we'll cover later in this tutorial) and the other that uses a free-form markup syntax (which we'll cover in the @seclink["third-tutorial"]{third tutorial}). In both cases, the basic process is the same: 1) compile the source into an X-expression, and then 2) convert that X-expression to the target file format using a template.

@subsection{X-expressions}

@(noskip-note)

I avoid nerdy jargon when I can. But in this case, the thing is called an @defterm{X-expression} throughout the Racket documentation, for good reasons. So I use the term too. Better to acclimate you now.

An X-expression is a way of representing markup-based data in code. X-expressions are indigenous to Lisp-based languages like Pollen and Racket. They don't exist in Python or JavaScript or Ruby.

Let's start with the part you're familiar with. By ``markup-based data,'' I mean things like HTML and XML and SVG. The idea is that you have text-based data surrounded by @defterm{tags}. Each tag can also have its own @defterm{attributes} that are made of keys and values. Tags can contain other tags, thus creating a tree-like structure. Right? You know what I mean:

@terminal{<body>
  <h1>Hello world</h1>
  <p class="first">Nice to <i>see</i> you.</p>
</body>}

An X-expression is just a simplified, generalized method of notation for these data structures — much like Markdown is a simplified method of notation for HTML. To see the relationship, we'll convert one into the other. 

First, we change the angle brackets to parentheses, and only use them on the outside of tags:

@terminal{(body 
  (h1 Hello world /h1) 
  (p class="first" Nice to (i see /i) you. /p) 
/body)}

Then we get rid of the closing tags, which are superfluous, since each closing parenthesis suffices to mark the end of a tag:

@terminal{(body
  (h1 Hello world)
  (p class="first" Nice to (i see) you.))}

However, this creates ambiguity between the name of the tag and the content. So we'll put the content within double quotes: 

@terminal{(body
  (h1 "Hello world") 
  (p class="first" "Nice to" (i "see") "you."))}

As for the @code{class} attribute, we need to distinguish it from both the markup tags and the content, so we'll move it into another pair of parentheses:

@terminal{(body
  (h1 "Hello world") 
  (p ((class "first")) "Nice to" (i "see") "you."))}

Whitespace isn't significant, so the X-expression can also be written without linebreaks:

@terminal{(body (h1 "Hello world") (p ((class "first")) "Nice to" (i "see") "you."))}

Skipping a few boring details, that's basically all there is to it. 

So why is it called an X-expression? Lisp languages are built out of units called S-expressions, which look like this:

@terminal{(and (txexpr? x) (memq (get-tag x) (setup:block-tags)))}

S-expressions use prefix notation, where each pair of parentheses contains a list. The first element in the list names a function, and the other elements are the arguments to that function. (This is a review of @secref["Racket_basics__if_you_re_not_familiar_"].) X-expressions just adapt S-expression notation to represent markup, hence the name (the @defterm{X} is short for @defterm{XML-like}).

For handling markup-based data, X-expressions have some clear advantages over other methods:

@itemlist[

@item{@bold{Readability.} X-expressions retain the semantics of markup-based data while dispensing with the infamous verbosity.}


@item{@bold{A hybrid between a tree and a string.} Most programming languages represent markup-based data either as a string or as an XML tree. Neither is a good choice. The string captures none of the internal structure of the data. An XML tree captures the structure, but conceals the sequential nature of the data elements. The X-expression shows both.}

@item{@bold{An ideal match for an expression-based programming language.} Aside from some notational details, X-expressions are just a subset of S-expressions generally, which are the building block of Racket. Processing X-expressions in Racket maximizes flexibility and minimizes @link["https://projects.csail.mit.edu/gsb/old-archive/gsb-archive/gsb2000-02-11.html"]{yak-shaving}.}
]

@margin-note{Given the close kinship between XML-ish data structures and Lisp-ish programming languages, I have no explanation why, during the Internet era, they have not been paired more often. They're like peanut butter and jelly.}

In Pollen's authoring modes, your source file is compiled into an X-expression, which is then injected into a template & converted to output. As a first example, we'll look at Markdown authoring mode.


@subsection{Markdown authoring mode}

Let's start putting together our multiple-page article. For simplicity, I'm going to use unrealistically short sample texts. But you can use whatever Markdown content you want.

We want to use Markdown authoring mode to make a file that will ultimately be HTML. So consistent with Pollen file-naming conventions (see @secref["Saving___naming_your_source_file"]), we'll start with our desired output filename, @filepath{article.html}, and then append the Markdown authoring suffix, which is @filepath{.pmd}. So in DrRacket, start a new file called @filepath{article.html.pmd} and put some Markdown in it: 

@fileblock["article.html.pmd"
@codeblock{
#lang pollen

Deep Thought
============

I am **so** happy to be writing this.
}]

Before you preview this file in the project server, click the @onscreen{Run} button in DrRacket just to see what the file produces. You'll see something like this:

@repl-output{
'(root
  (h1 ((id "deep-thought")) "Deep Thought")
  (p "I am " (strong "so") " happy to be writing this."))
}

You should now be able to recognize this as an X-expression. In authoring mode, Pollen compiles your Markdown into the corresponding HTML entities, but then provides the data as an X-expression rather than finished HTML.

From what you learned in the last section, it should be evident that this X-expression will convert to HTML that looks like this:

@terminal{
<root>
  <h1 id="deep-thought">Deep Thought</h1>
  <p>I am <strong>so</strong> happy to be writing this.</p>
</root>
}

``But what's this @code{root} tag? That's not HTML.'' An X-expression that holds other X-expressions must have a root tag. So in the spirit of obviousness, every X-expression produced by Pollen in authoring mode will start with @code{root}. If you don't need it, you can discard it (we'll cover this below, in  @secref[#:tag-prefixes '("tutorial-2")]{Templates}). Though as you'll learn in the @seclink["third-tutorial"]{third tutorial}, @code{root} also creates a useful hook for further processing — it's not a superfluous accessory.


@subsection{Review: authoring mode vs. preprocessor mode}

Before we proceed, let's quickly review how authoring mode is different from preprocessor mode. To do that, let's take the same Markdown content, but this time put it into a preprocessor source file called @filepath{article.md.pp}.

@fileblock["article.md.pp"
@codeblock{
#lang pollen

Deep Thought
============

I am **so** happy to be writing this.
}]

When you run this file in DrRacket, you'll see:

@repl-output{
Deep Thought
============

I am **so** happy to be writing this.
}

This result makes sense, right? To recap: when you use Markdown source in preprocessor mode, Pollen gives you Markdown. When you use Markdown source in authoring mode, Pollen gives you an X-expression.

So how do you convert an X-expression into HTML? Read on.

@section[#:tag-prefix "tutorial-2"]{Templates}

In Pollen, a @defterm{template} lets you convert an X-expression to your target output format. If you've used other web-publishing systems, templates are probably a familiar idea. Templates in Pollen are both similar and different.

First, the two major similarities:

@itemlist[

@item{At its simplest, a template holds @bold{boilerplate material} that you want to reuse across multiple output files. For instance, in a set of HTML pages, you might have layout and navigation elements that stay the same, while the content changes. In that case, you could put the layout and navigation in the template, and keep the content in your Pollen source files. When you want to add a new page, you can make a new source file and just use it with the existing template. Moreover, if you want to change the layout and navigation globally, you can just change the template, rather than changing the source files.}

@item{Pollen templates, like others, can also have @bold{conditional elements} — meaning, you can embed simple code in your templates that allows them to change based on the content in the page. For instance, a template could show or hide ``previous page'' and ``next page'' links depending on whether there's actually a previous or next page. (Jump ahead to the @seclink["third-tutorial"]{third tutorial} if this excites you.)}

]

And two major differences:

@itemlist[

@item{There's @bold{no special template language} you need to learn, with magic syntax and whatnot. Instead, you can use all the same Pollen commands in a template that you can in authoring mode or preprocessor mode.}

@item{Within a template, you have to @bold{explicitly convert} the X-expression to the target format. This is a feature, not a bug. By avoiding assumptions about the target format, Pollen templates can be used to generate any kind of file (even binary formats like PDF). But the cost of this flexibility is that you need to tell Pollen what you want.}
]



@margin-note{``So a template is also a Pollen source file?'' Not quite. More accurately, it's a fragment of Pollen source that is completed by adding the X-expression that comes out of one of your source files. Because of this, there are a few extra limitations on the code you can put in a template, though with easy workarounds.}


To see how this works, let's return to the source file we started in the last section:

@fileblock["article.html.pmd"
@codeblock{
#lang pollen

Deep Thought
============

I am **so** happy to be writing this.
}]

Last time, I had you run this file in DrRacket to see the X-expression it produced. This time, load it in the project server. You'll see something like this:

@browser{
@bold{@larger{Deep Thought}}

I am @bold{so} happy to be writing this.
}

Here, you're seeing the X-expression from your source combined with an HTML template, which adds the necessary boilerplate for the finished HTML:

@terminal{
<html><head><meta charset="UTF-8" /></head><body><root><h1 id="my-article">Deep Thought</h1><p>I am <strong>so</strong> happy to be writing this.</p></root>
</body></html>
}

But wait — where did the template come from? If you try to render a file to HTML without first setting up a template, Pollen helps you out and uses its @defterm{fallback template} for HTML. The fallback template is just a minimal template that's used as a last resort. Under ordinary circumstances, seeing the fallback template probably signals a problem (e.g., Pollen couldn't find the template you asked for). For now, we can rely on it to get a preview of our file.

Still, we can learn a few things about how to make an HTML template by studying the fallback template.

@subsection{The @tt{doc} export and the @tt{->html} function}

To understand the necessary parts of a template, let's look at a simple one — the fallback template that Pollen uses for HTML files, called @filepath{fallback.html}.

@fileblock["fallback.html"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
◊(->html (html (head (meta #:charset "UTF-8")) (body doc)))
}]

The fallback template has three key ingredients: 

The first ingredient is an X-expression that represents a basic HTML page:

@codeblock[#:keep-lang-line? #f]{
#lang pollen
(html (head (meta #:charset "UTF-8")) (body))
}

That X-expression is equivalent to this HTML string:

@terminal{<html><head><meta charset="UTF-8"></head><body></body></html>}

But how do we get from X-expression to HTML? Here's how: within the template, we'll explicitly convert this X-expression to HTML. We do this by sending the X-expression through our second key ingredient, the function @racket[->html]:

@codeblock[#:keep-lang-line? #f]{
#lang pollen
◊(->html (html (head (meta #:charset "UTF-8")) (body)))
}

@racket[->html] is in the @racketmodname[pollen/template] module, but this module is automatically imported into every template, so there's nothing extra we need to do.

Finally, we need to include the X-expression from our source file. By convention, every Pollen source file makes its output available through an exported variable called @code{doc}. A source file in preprocessor mode puts its text result in @code{doc}. And a source file in authoring mode puts its X-expression result in @code{doc}. So we put the third key ingredient, the variable @code{doc}, inside our @code{body} tag.

@codeblock[#:keep-lang-line? #f]{
#lang pollen
◊(->html (html (head (meta #:charset "UTF-8")) (body doc)))
}

To summarize: this template contains a skeletal HTML page (in X-expression format). We drop @code{doc} into the template to indicate where the X-expression of our source file should be inserted. Finally, we convert the whole X-expression to HTML with @racket[->html].

``So I have to convert my HTML template to an X-expression?'' No. That's optional. You can also put hard-coded HTML in your template. Here's an equivalent way of writing @filepath{fallback.html.p}, with explicit HTML:

@fileblock["fallback.html.p" @codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head><meta charset="UTF-8"></head>
<body>◊(->html doc)</body>
</html>
}]

Notice that we still need to use the @racket[->html] function, but this time, instead of surrounding a larger X-expression, it just goes around @code{doc}. The rest of the template code gets passed through as plain text.

What matters most is ending up with HTML. Otherwise, there is no difference between these two approaches. In the first, you're describing the whole template using an X-expression and converting the whole thing with @racket[->html]. In the second, you're hard-coding the boilerplate HTML, so the only part that needs to be converted with @racket[->html] is @code{doc}. 

Use whichever method works best for you. I often prefer the second method, because I like to build HTML layouts by hand using placeholder content to make sure all the fiddly bits work. Then it's easy to replace the placeholder content with @racket[(->html doc)], and it becomes a template.

On the other hand, when you use X-expressions to generate HTML, the result is always perfect. You'll never have to comb through a template looking for a missing closing @code{</div>} tag.

@subsection{Making a custom template}

Having learned about the three key template ingredients, we'll now make a custom template for @filepath{article.html.pmd}. 

In general, template files can have any name you want. But by default, Pollen will first look for a file in your project directory called @filepath{template.ext}, where @filepath{ext} matches the output-file extension of the source file. So if your source file is @filepath{database.xml.pmd}, Pollen will look for @filepath{template.xml}. And for @filepath{article.html.pmd}, Pollen will look for @filepath{template.html}.

@margin-note{If you want to use a template with a different name, or apply a special template to a particular source file, you can specify a template from within any source file by inserting the line @code{◊(define-meta template "my-template-name.html")}. For more, see @racket[get-template-for].}

Furthermore, it's also fine to use a Pollen source file that will result in @filepath{template.html}. Following the fallback-template example above, we'll use the null extension and name our template @filepath{template.html.p} — Pollen will convert this to the @filepath{template.html} it needs.

Beyond that, all we need to do make sure our template has the three key ingredients we saw in the fallback template. Pollen will automatically apply it to @filepath{article.html.pmd} when we view it in the project server.

In your project directory, create a new file called @filepath{template.html.p}:

@margin-note{If you're using DrRacket on Mac OS to save this file, it may insist on adding a @filepath{rkt} extension to the filename. If so, you can either correct the filename after you save the file, or instead use a different text editor to create @filepath{template.html.p}.} 


@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>Custom template</title>
</head>
<body>◊(->html doc)</body>
</html>
}]

The @filepath{p} extension is Pollen's @seclink["Null___p_extension_"]{@defterm{null extension}}, which does what you might guess — nothing. The template would still work if it were called @filepath{template.html}. But using the null extension is a virtuous habit, because it keeps your templates distinct from output files (both for your sake, and your operating system, which may get flummoxed by things like @filepath{template.pdf}, which would be a text-based file with a binary extension.

Otherwise, this is the same as the fallback template we saw in the previous section, but written out in HTML, and with a @code{title} element added. Refresh @filepath{article.html} in the project server. Does it look different? No — it won't, because the template is basically the same. But because of the new @code{title} field, you should notice that the title shown in the browser window is @onscreen{Custom template}. This shows that Pollen is correctly relying on our new template file, rather than the fallback template.

Let's keep going, and add a @code{style} block to our custom template:

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>Custom template</title>
  <style type="text/css">
    body {padding: 3em; font-size: 20px;}
    h1 {background: gray; color: white;}
    strong {color: red;}
  </style>
</head>
<body>◊(->html doc)</body>
</html>}]

Refresh @filepath{article.html} in the project server again. Now you'll see that the heading has a gray background, and one word in the text is red.

Feel free to add other settings to @filepath{template.html.p}, or update the text in @filepath{article.html.pmd}, and see how the page changes in the browser. As you'd expect, the project server keeps an eye on both your source files and your template files, and if one changes, it regenerates the output file.

@subsection{Inserting specific source data into templates}

In the previous example, we used @code{doc} to insert the entire content of the source file — as an X-expression — into the template.

But what if you want to only insert part of your source file into the template? For instance, you'll look like a dork if the title on each page is @onscreen{Custom template}. So let's fix that.

When you're working in a template, Pollen provides a @racket[select] function that lets you extract the content of a specific tag, like so: @code{◊(select tag-name doc)}, which means ``get the content of @racketvarfont{tag-name} out of @code{doc} and put it here.''

Let's suppose that we'd rather use the name of the article — @italic{Deep Thought} — as the page title. To do this, we'll put a @racket[select] command inside the @code{<title>} tag. 

To make @racket[select] work, we also need to know the tag name that contains the title. If we have a little Markdown expertise, we might already know that this part of our Markdown source:


@codeblock[#:keep-lang-line? #f]{
#lang pollen
Deep Thought
============
}

is going to produce a tag named @code{h1}. 

What if we don't have all the Markdown conversions memorized? No problem. We can still figure out the tag name by running the @filepath{article.html.pmd} source file in DrRacket and looking at the resulting X-expression:

@repl-output{'(root (h1 ((id "my-article")) "Deep Thought") (p "I am " 
(strong "so") " happy to be writing this."))}

Either way, now we know that the text @italic{Deep Thought} lives in the @code{h1} tag. So we update our template accordingly (for brevity, I'm going to omit the @code{style} tag in these examples, but it's fine to leave it in):

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'h1 doc)</title>
</head>
<body>◊(->html doc)</body>
</html>
}]

When you refresh the page in the project server, the page title will now appear as @onscreen{Deep Thought}. Of course, you can also combine static and dynamic elements in your template, like so:

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'h1 doc), by MB</title>
</head>
<body>◊(->html doc)</body>
</html>
}]

The page title will now be @onscreen{Deep Thought, by MB}.

A couple notes on command syntax. We wrote the @racket[select] and @racket[->html] commands in Racket style. We could also write the commands in Pollen style, like so:

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊select['h1 doc], by MB</title>
</head>
<body>◊->html[doc]</body>
</html>
}]

This is exactly equivalent to the previous example. Skeptics are welcome to confirm this by reloading the page in the project server.

Finally, notice that in the @racket[select] command, the tag name @code{h1} is written with a quote mark (@code{'h1}), whereas @code{doc} is not. This is an easy place to get tripped up, but the rule is simple: you don't use a quote mark when you're referring to an @defterm{identifier} — that is, the name of an existing function or variable (like @racket[select] or @code{doc}). But you do need a quote mark when you're using the thing as a literal value.


@margin-note{Racket (and hence Pollen) makes a distinction between @secref["symbols" #:doc '(lib "scribblings/guide/guide.scrbl")] (e.g. @racket['h1]) and @secref["strings" #:doc '(lib "scribblings/reference/reference.scrbl")] (e.g.  @racket["h1"]). Without getting into the weeds, just know for now that the tag of an X-expression is always a symbol, not a string. But if you write @racketfont*{◊(@racket[select] "h1" doc)}, the command will still work, because Pollen will treat it as @racketfont*{◊(@racket[select] 'h1 doc)}, consistent with a general policy of not being persnickety about input types when the intention is clear.}


@subsection{Linking to an external CSS file}

If you're a super web hotshot, you probably don't put your CSS selectors in the @code{<head>} tag. Instead, you link to an external CSS file. So it will not surprise you that in Pollen, you can do this by adding the usual @code{<link>} tag to your HTML template, in this case a file called @filepath{styles.css}:

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'h1 doc), by MB</title>
  <link rel="stylesheet" type="text/css" href="styles.css" />
</head>
<body>◊(->html doc)</body>
</html>
}]

Fans of hand-coded CSS, I trust you to take it from here: put a @filepath{styles.css} file in your project directory, and enjoy the results.

But folks who paid attention during the @seclink["first-tutorial"]{first tutorial} might be wondering ``Can we link to a dynamically generated @filepath{styles.css.pp} file?''

Yes, of course. Here's the rule of thumb: when you're making links between files — whether CSS, HTML, or anything else — Pollen doesn't care whether the file is static or dynamic. You just refer to it by its ultimate name, in this case @filepath{styles.css}. If a static @filepath{styles.css} file exists, Pollen will use that. If it doesn't, Pollen will look for a source file it can use to make @filepath{styles.css}, and generate it on the spot. (You can also start with a static file, and change it to be dynamic later, and Pollen will do the right thing.)

So to use a dynamic CSS file, we don't need to make any changes to @filepath{template.html.p}. We just need to add @filepath{styles.css.pp} to the project directory:

@fileblock["styles.css.pp"
@codeblock{
#lang pollen

◊(define h1-color "blue")
◊(define strong-color "green")

body {padding: 3em; font-size: 20px;}
h1 {background: ◊|h1-color|; color: white;}
strong {color: ◊|strong-color|;}
}]

This time, when you refresh @filepath{article.html} in the project server, Pollen will run @filepath{styles.css.pp} to get the @filepath{styles.css} file it needs, and you'll see the new colors in the page. As usual, if you update @filepath{styles.css.pp}, Pollen will notice and regenerate the CSS file when you refresh the page.

Can you add multiple dynamic style sheets? Yes. 
@(linebreak)Can you mix dynamic and static style sheets? Yes.
@(linebreak)Can you add a dynamic JavaScript file? Yes.

You've got the general idea, right? So let's move on.


@section[#:tag-prefix "tutorial-2"]{Intermission}

If you only need one page for your article, you can stop here. You now know everything necessary to publish a single-page article using authoring mode. You know how to create the mandatory ingredients — a source file and a template — and  you also know how to link to an optional CSS file, which can be dynamically generated.

If you want to create a multi-page article, however, you need to get through one more big idea. This might be a good time to take a break.

@section[#:tag-prefix "tutorial-2"]{Pagetrees}

A @italic{pagetree} is a hierarchical list of Pollen pages. When you have multiple pages in your project, the pagetree establishes relationships among those pages. At its most basic, a pagetree establishes a linear sequence for the pages. But pagetrees can also establish hierarchical relationships — for instance, a book-length project can be organized into chapters, the chapters into sections, and so on. The pagetree doesn't impose any semantics on the organization of your project. It's just a tree, and it's up to you how many layers to establish, what those layers mean, and so on.


@subsection{Pagetree navigation}

An obvious use for a pagetree is to add navigational links to your pages. Obviously, in a multi-page article, readers need a way of getting from one page to the next. In this part of the tutorial, we'll expand our sample article from one page to three, and see how to create ``previous page'' and ``next page'' links in our template that are dynamically generated relative to the current page.


@subsection{Using the automatic pagetree}

You've actually already been exposed to pagetrees (though I didn't tell you about it at the time). Recall that the dashboard of the project server is located at @link-tt{http://localhost:8080/index.ptree}. The list of files you see in the dashboard is a pagetree that Pollen generates by getting a list of files in the current directory and arranging them in alphabetical order.

Thus, if the multiple pages in your project are already ordered alphabetically by filename, then you can rely on this automatic pagetree. (More commonly, you'll make a separate pagetree file for the navigation — but we'll cover that later in this tutorial.)

From earlier in the tutorial, you have a Markdown source file called @filepath{article.html.pmd} that looks like this:

@fileblock["article.html.pmd"
@codeblock{
#lang pollen

Deep Thought
============

I am **so** happy to be writing this.
}]

Let's supplement this source file by creating two others for the project:

@fileblock["barticle.html.pmd"
@codeblock{
#lang pollen

Barticle Title
==============

The wonderful second part of the article.
}]

@fileblock["carticle.html.pmd"
@codeblock{
#lang pollen

Carticle Title
==============

The terrific third part.
}]

As before, you can fill these source files with any sample Markdown content you like. Moreover, you don't have to use the filenames @filepath{barticle.html.pmd} and @filepath{carticle.html.pmd} — the point is that the intended sequence needs to match the alphabetic sorting of the filenames.

We'll reuse the @filepath{template.html.p} and @filepath{styles.css.pp} files from earlier in the tutorial. Move or delete the other tutorial files so that your dashboard in the project server shows only these five files:

@itemlist[

@item{@filepath{article.html.pmd}}
@item{@filepath{barticle.html.pmd}}
@item{@filepath{carticle.html.pmd}}
@item{@filepath{styles.css.pp}}
@item{@filepath{template.html.p}}
]

If you click on any of the three Markdown sources, you'll see it converted into HTML using @filepath{template.html.p}, with styles from the generated @filepath{styles.css}.

The automatic pagetree for this project is exactly what you see in the dashboard: a list of the three article files, followed by @filepath{styles.css} and @filepath{template.html.p}.

@subsection{Adding navigation links to the template with @tt{here}}

Recall from earlier in the tutorial that the content of your source file is made available in the template through the special variable @code{doc}. Likewise, the output name of the current source file is made available through the special variable @code{here}. 

To make any navigation link — up, down, sideways — the general idea is that we use @code{here} as input to a pagetree-navigation function, which then looks up the answer in the current pagetree.

First, let's just see @code{here} on its own. Update your template as follows:

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'h1 doc), by MB</title>
  <link rel="stylesheet" type="text/css" href="styles.css" />
</head>
<body>◊(->html doc)
The current page is called ◊|here|.
</body>
</html>
}]

If you refresh @filepath{article.html}, you'll now see the line ``The current page is called article.html.'' Switch to @filepath{barticle.html}, and you'll see ``The current page is called barticle.html.'' Makes sense, right? 

Notice that @code{here} is always an @italic{output} filename, on the idea that navigation naturally makes connections among output files, not source files. In this case, this conversion to output name means we lose the @filepath{pmd} extension. (As we'll see below, we'll also use these output filenames when we're @secref["Making_a_pagetree_file"].)

Now let's use pagetree functions to show the names of the previous and next pages. Consistent with the usual Pollen policy of obviousness, these functions are called @racket[previous] and @racket[next]:

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'h1 doc), by MB</title>
  <link rel="stylesheet" type="text/css" href="styles.css" />
</head>
<body>◊(->html doc)
The current page is called ◊|here|.
The previous is ◊|(previous here)|. 
The next is ◊|(next here)|.
</body>
</html>
}]

Refresh @filepath{barticle.html}. You'll now see that ``The current page is called barticle.html. The previous is article.html. The next is carticle.html.'' So far, so good: we're correctly deriving the previous and next pages from the automatic pagetree.

All that's left is adding the hyperlinks:

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'h1 doc), by MB</title>
  <link rel="stylesheet" type="text/css" href="styles.css" />
</head>
<body>◊(->html doc)
The current page is called ◊|here|.
The previous is <a href="◊|(previous here)|">◊|(previous here)|</a>. 
The next is <a href="◊|(next here)|">◊|(next here)|</a>.
</body>
</html>
}]

Refresh @filepath{barticle.html}, and you'll see that the names of the previous and next pages are now hyperlinks to those pages. Click through and convince yourself that it works.

@margin-note{The documentation for pagetree @secref["Navigation"] will tell you about the other functions available for generating navigation links.}

@subsection{Handling navigation boundaries with conditionals}

If you clicked through to @filepath{article.html} or @filepath{carticle.html}, you might've noticed a couple problems. Because @filepath{article.html} is the first page in the automatic pagetree, it doesn't have any previous page it can link to. And the next-page link for @filepath{carticle.html} is @filepath{styles.css}, which is strictly correct — it is, in fact, the next file in the automatic pagetree — but it's not part of our article, so we'd rather stop the navigation there.

One way to fix the problem would be to have three separate template files — the standard one with both previous- and next-page links, one with only a next-page link, and one with only a previous-page link. 

But since we have a whole programming language available in Pollen, that's a weak solution. The better way is to add @defterm{conditionals} to the template to selectively change the navigation. That keeps things simple, because we'll still have only one @filepath{template.html.p} to deal with.

To handle @filepath{article.html}, we want to hide the previous-page navigation link when there's no previous page. As it turns out, if the @racket[previous] function can't find a previous page, it will return false. So we just need to wrap our previous-page navigation in the @racket[when/splice] command like so:

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'h1 doc), by MB</title>
  <link rel="stylesheet" type="text/css" href="styles.css" />
</head>
<body>◊(->html doc)
The current page is called ◊|here|.
◊when/splice[(previous here)]{The previous is 
<a href="◊|(previous here)|">◊|(previous here)|</a>.} 
The next is <a href="◊|(next here)|">◊|(next here)|</a>.
</body>
</html>
}]

The basic form of @racket[when/splice] is 

@racketfont{◊when/splice[@racketvarfont{test-condition}]{@racketvarfont{content-to-insert}}} 

We're writing this command in Pollen style — note the square braces around the @racketvarfont{condition}, and the curly braces around the @racketvarfont{text}. Using @racket[(previous here)] as the condition is shorthand for ``when @racket[(previous here)] does not return false...''

Programmers in the audience might be getting anxious about the repeated use of @racket[(previous here)] — you're welcome to store that value in a variable, and everything will work the same way:

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'h1 doc), by MB</title>
  <link rel="stylesheet" type="text/css" href="styles.css" />
</head>
<body>◊(->html doc)
The current page is called ◊|here|.
◊(define prev-page (previous here))
◊when/splice[prev-page]{The previous is 
<a href="◊|prev-page|">◊|prev-page|</a>.} 
The next is <a href="◊|(next here)|">◊|(next here)|</a>.
</body>
</html>
}]

We need a different technique for handling the end of the next-page navigation, because we're not reaching the actual end of the pagetree. We're just reaching the end of the pages we care about navigating through. 

What condition will help us detect this? This time, we might notice that the names of our article pages all contain the string @code{article}. In a real project, you'd probably want a more meaningful test condition. But in this tutorial, what we'll do is hide the next-page navigation if the name of the next page doesn't contain @filepath{article}. As we did before, we wrap our navigation line in the @racket[when/splice] function:

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'h1 doc), by MB</title>
  <link rel="stylesheet" type="text/css" href="styles.css" />
</head>
<body>◊(->html doc)
The current page is called ◊|here|.
◊(define prev-page (previous here))
◊when/splice[prev-page]{The previous is 
<a href="◊|prev-page|">◊|prev-page|</a>.} 
◊when/splice[(regexp-match "article" (symbol->string (next here)))]{
The next is <a href="◊|(next here)|">◊|(next here)|</a>.}
</body>
</html>
}]

This time, the condition is @racket[(regexp-match "article" (symbol->string (next here)))]. How were you supposed to know this? You weren't. That's why this is a tutorial. Without going on a lengthy detour, the @racket[regexp-match] function is Racket's regular-expression matcher. It returns true if the first string (in this case, @racket["article"]) is found inside the second string (in this case, we convert @racket[(next here)] to a string by wrapping it in @racket[symbol->string]).

Even if some of the programmy bits went over your head just now, relax and paste the code into your template. What you'll see when you refresh @filepath{carticle.html} is that the next-page link is gone. So now our template lets us navigate among the pages of our article, and the conditionals handle the end pages correctly.

@subsection{Making a pagetree file}

I didn't want to dwell on programming complications in the previous section. Why? Because the extra programming was necessary only because we made life somewhat difficult for ourselves by relying on the automatic pagetree. 

A better way to solve the problem is to avoid it altogether by making a pagetree file.

Pagetree files have a different syntax and status than other Pollen source files, so they're compiled using their own Pollen dialect. To invoke this dialect, you just start the file with @tt{#lang pollen} and name the file with the @filepath{ptree} extension, for instance @filepath{my-project.ptree}. While you can have as many pagetrees in your project as you want, Pollen will first look for one named @filepath{index.ptree}.

@margin-note{A pagetree file only gets one file extension, because unlike other Pollen source files, it's never converted into an output file.}

So let's make an @filepath{index.ptree} file. At its simplest, a pagetree file can just be a list of files in the intended order. In DrRacket, create a new file in your project directory as follows:

@fileblock["index.ptree"
@codeblock{
#lang pollen

carticle.html
article.html
barticle.html
}]

Now run the file. The result will be:

@repl-output{'(pagetree-root carticle.html article.html barticle.html)}

Pretty boring, I know. But behind the scenes, Pollen's pagetree compiler is making sure your tree is valid (e.g., no duplicate or malformed names). Today it's boring, but on the day you have a long and complicated pagetree, you'll be grateful.

Notice that the names in this pagetree are the names of @italic{output} files, not source files. This is deliberate. Neither you nor Pollen has to care which files are static vs. dynamic. This next pagetree wouldn't be wrong in the sense of bad syntax — the pagetree compiler won't complain — but it would be wrong in the sense of not-what-you-want, because it refers to source names rather than output names:

@fileblock["bad-index.ptree"
@codeblock{
#lang pollen

carticle.html.pmd
article.html.pmd
barticle.html.pmd
}]

You also probably noticed that the files are in a different order than they were in the automatic pagetree: @filepath{carticle.html} is first, followed by @filepath{article.html} and then @filepath{barticle.html}. This too is deliberate, so we can see what happens with a differently ordered pagetree.

Pagetrees don't change as often as other source files, so as a performance optimization, the project server ignores them when deciding whether to refresh a file. Thus, after updating a pagetree, you have to manually reset your project. Go to your terminal window and stop the project server with @onscreen{ctrl+C}. 

Then clear Pollen's cache of rendered pages:

@terminal{
> raco pollen reset}

And restart the project server:

@terminal{
> raco pollen start}

Now click on @filepath{carticle.html} again. You'll notice that the navigation links are different. You won't see a previous-page link — because @filepath{carticle.html} is now the first page in the pagetree — and the next page will show up as @filepath{article.html}. Click through to @filepath{article.html}, and you'll see the navigation likewise updated. Click through to @filepath{barticle.html}, and you'll see ...

BAM! An error page that says 

@errorblock{symbol->string: contract violation
  expected: symbol?
  given: #f}

  What happened? We switched to using our own pagetree file but we didn't update the conditions in our template. Once we reach @filepath{barticle.html}, the value of @racket[(next here)] is false (@racket[#f]). But the @racket[(symbol->string (next here))] command in the template needs a symbol as input. Hence the error.

 So let's go back and fix that. Because we don't have extraneous files in our pagetree anymore, we can change the second conditional in the template to work the same way as the first:

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'h1 doc), by MB</title>
  <link rel="stylesheet" type="text/css" href="styles.css" />
</head>
<body>◊(->html doc)
The current page is called ◊|here|.
◊(define prev-page (previous here))
◊when/splice[prev-page]{The previous is <a href="◊|prev-page|">◊|prev-page|</a>.} 
◊(define next-page (next here))
◊when/splice[next-page]{The next is <a href="◊|next-page|">◊|next-page|</a>.}
</body>
</html>
}]

Refresh @filepath{barticle.html} — because you're updating the template, you don't need to restart the project server — and you'll see the right result. The previous-page link goes to @filepath{article.html}, and the next-page link is hidden.

@subsection{@tt{index.ptree} & the project server}

One more thing before we wrap up this tutorial. Remember that the dashboard of the project server is at @link-tt{http://localhost:8080/index.ptree}? By default, the project server will generate a pagetree from an alphabetical directory listing. 

But if you put your own @filepath{index.ptree} file in that directory, the project server will use that for the dashboard instead. In fact, visit @link-tt{http://localhost:8080/index.ptree} now and you'll see what I mean. Consistent with the @filepath{index.ptree} you made, you'll now see @filepath{carticle.html}, @filepath{article.html}, and @filepath{barticle.html}, but not @filepath{template.html.p} nor @filepath{styles.css} (even though they're still in the project directory).


@section{Second tutorial complete}

That was a big tutorial. I commend you for your tenacity and patience. But you made a giant leap forward. Despite the silly examples, you now know everything you need to make multi-page articles — books, even — using Markdown authoring mode in Pollen. If this is all you ever use Pollen for, it'll be a big improvement over ordinary Markdown.

But there's more. We haven't even gotten into the more elaborate automation that's possible with Pollen, nor Pollen's own markup language. We'll cover that in the @seclink["third-tutorial"]{third tutorial}.


