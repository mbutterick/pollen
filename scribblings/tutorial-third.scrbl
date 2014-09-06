#lang scribble/manual

@(require scribble/eval (for-label plot pollen/world pollen/tag racket/base pollen/template txexpr racket/list racket/string))
@(require "mb-tools.rkt")

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/decode pollen/template pollen/tag xml racket/list txexpr))


@title[#:tag "third-tutorial"]{Third tutorial}

Now you're getting to the good stuff. In this tutorial, you'll use Pollen to publish a multi-page article written in Pollen markup. You'll learn about:

@itemlist[


@item{Adding tags & attributes with Pollen markup}

@item{Attaching behavior to tag functions}

@item{the @racketfont{directory-require.rkt} file}

@item{@exec{raco pollen render} and @exec{raco pollen clone}}


]

If you want the shortest possible introduction to Pollen, try the @secref["quick-tour"].

@section[#:tag-prefix "tutorial-3"]{Prerequisites}

I'll assume you've completed the @secref["second-tutorial"] and that you understand the principles of Pollen authoring mode — creating source files, converting them to X-expressions, and then combining them with templates to make output files. 

Because now it's time to pick up the pace. You've learned how to do some handy things with Pollen. But we haven't yet exploited the full fusion of writing environment and programming language. I promised you that @secref["The_book_is_a_program" #:doc '(lib "pollen/scribblings/pollen.scrbl")], right? So let's do some programming.


@section{Pollen markup vs. XML}

You can skip this section if XML holds no interest. But Pollen markup evolved out of my attempt to come up with an alternative to XML that would be more usable for writing. So if you're familiar with XML, the contrast may be helpful. 

@subsection{The XML problem}

In the @secref["second-tutorial"], I made the case that Markdown is a limiting format for authors. Why? Markdown is essentially a notation system for HTML tags. As such, it has three problems: it's not semantic, it only covers a limited subset of HTML tags, and it can't be extended by an author. 

These problems are partly limitations of HTML itself. And these limitations were meant to be cured by XML — the @italic{X} stands for @italic{extensible}. In principle, XML allows you to define whatever tags you like and use them in your document.

So why hasn't XML taken over the world? In practice, XML promises more than it delivers. The reasons are apparent to any writer who's attempted to use XML as an authoring format:

@itemlist[

@item{@bold{Verbose syntax}. Unfortunately, XML relies on the same angle-bracket notation as HTML. If you think HTML source is hard to read, XML is even worse. Since much of writing involves reading, this feature is also a major bug.}

@item{@bold{Validation overhead}. Integral to XML is the concept of @italic{validation}, which guarantees that a document meets certain formal criteria, usually asserted in a @italic{schema}. To get the full value from XML, you generally want to use validation. But doing so imposes a lot more work on you as an author, and removes much of the expressive potential of XML.}

@item{@bold{Masochistic document processing}. I'm referring to XSLT, the preferred method of transforming XML documents. I know a little XSLT, so I'll concede that there's a method to its madness. But it's still madness.}

]

The nicest thing we could say about XML is that its intentions are good. It's oriented toward the right goals. But its benefits are buried under atrocious ergonomics.


@subsection{What Pollen markup does differently}

Pollen markup can be seen as a way of reaping the benefits of XML without incurring the headaches. Like XML, Pollen markup allows you to freely tag your text. But unlike XML:

@itemlist[

@item{@bold{Simple syntax}. Pollen markup follows the usual conventions of Pollen commands.}

@item{@bold{No structural validation}. You can use any tags you want, in any order, and you needn't define them ahead of time. Your document will still work.}

@item{@bold{Racket processing}. Pollen markup tags can have behavior attached to them using Racket functions, either before you use them, or later.}

]



@subsection{``But I really need XML…''}

You can have XML. There's nothing wrong with using Pollen markup to generate XML files that can then be fed into an existing XML processing pipeline. In other words, using Pollen markup, you can treat XML as an output format rather than an input format. 

In this tutorial, I'll be rendering Pollen markup with an HTML template. But you could easily use the same workflow with an XML template and thus end up with XML files.


@section{Writing with Pollen markup}

Pollen markup is a free-form markup system that lets you add arbitrary @italic{tags} and @italic{attributes} to your text. By arbitrary, I mean that they don't need to match up with an existing schema or specification (e.g., the tags permitted by HTML). They can — but that's an option, not a requirement. 

I like to think of Pollen markup a way of capturing not just the text, but also my @bold{ideas about the text}. Some of these are low-level ideas (``this text should be italicized''). Some are high-level ideas (``this text is the topic of the page''). Some are just notes to myself. In short, everything I know about the text becomes part of the text. 

In so doing, Pollen markup becomes the source code of the book. Let's try it out.

@subsection{Creating a Pollen markup file}

We're going to use Pollen markup to make a file that will ultimately be HTML. So consistent with the authoring-mode workflow we learned in the @secref["second-tutorial"], we'll start with our desired output filename, @racketfont{article.html}, and then append the Pollen markup suffix, @racketfont{.pm}.

In DrRacket, start a new file called @racketfont{article.html.pm} like so (BTW you can use any sample text you like):

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
I want to attend RacketCon this year.
}]

Consistent with usual authoring-mode policy, when you run this file, you'll get an X-expression that starts with @racketvalfont{root}:

@repl-output{'(root "I want to attend RacketCon this year.")}

Remember, even though the first line of the file is @racketmodfont{#lang} @racketmodname[pollen] — same as the last tutorial — the new @racketfont{.pm} suffix signals that Pollen should interpret the source as Pollen markup. Look what happens if you goof up and put Markdown source in a Pollen markup file, like so:

@codeblock{
#lang pollen
 
I am **so** excited to attend __RacketCon__ this year.
}

The Markdown syntax will be ignored, and pass through to the output:

@repl-output{'(root "I am **so** excited to attend __RacketCon__ this year.")}

Restore the non-Markdown source, and let's continue.


@subsection{Tags & tag functions}

Pollen markup uses the same Pollen command syntax that we first saw in @secref["Adding_commands" #:doc '(lib "pollen/scribblings/pollen.scrbl")]. Previously, we used this command syntax to invoke functions like @racket[define] and @racket[->html]. Pollen markup is used to invoke a special kind of function called a @italic{tag function}, which is a function that, by default, adds a tag to the text.

To see how this works, restore your @racketfont{article.html.pm} file to its original state:

@codeblock{
#lang pollen
 
I want to attend RacketCon this year.
}

We can add any tag with Pollen markup, but for now, let's start with an old favorite: @racketvalfont{em}, which is used in HTML to add emphasis to text. We apply a tag by starting with the lozenge character (◊) followed by the tag name @racketvalfont{em}, followed by the text in curly braces, like so:

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
I want to attend ◊em{RacketCon this year}.
}]

Run this file in DrRacket and see the X-expression that results:

@repl-output{'(root "I want to attend " (em "RacketCon this year") ".")}


You won't be surprised to hear that you can nest tags:

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
I want to attend ◊em{RacketCon ◊strong{this} year}.}]

With the expected results:

@repl-output{'(root "I want to attend " (em "RacketCon " (strong "this") " year") ".")}

@subsection{Attributes}

@italic{Attributes} are like tags for tags. Each attribute is a key–value pair where the key is any name, and the value is a string. Anyone who's seen HTML is familiar with them:

@repl-output{<span class="author">Prof. Leonard</span>}

Here, @racketvalfont{class} is an attribute for @racketvalfont{span} that has value @racketvalfont{"author"}. And this is what it looks like as an X-expression:

@repl-output{'(span ((class "author")) "Prof. Leonard")}

You can add any number of attributes to a tag (first as an X-expression, then as HTML):

@repl-output{'(span ((class "author")(id "primary")(living "true")) "Prof. Leonard")}

@repl-output{<span class="author" id="primary" living="true">Prof. Leonard</span>}

In Pollen markup, attributes have the same logic, but a slightly different syntax. In keeping with the tag notation you just saw, the @racketvalfont{span} tag is added in the usual way:

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
◊span{Prof. Leonard}}]

Then you have two options for adding attributes. The verbose way corresponds to how the attributes appear in the X-expression:

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
◊span['((class "author")(id "primary")(living "true"))]{Prof. Leonard}
}]

Each key–value pair is in parentheses, and then the list of pairs is within parentheses, with a @racket[quote] (@litchar{'}) at the front that signals that the text should be used literally.

This involves some superfluous typing, however, so Pollen also supports an abbreviated syntax for attributes:

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
◊span['class:"author" 'id:"primary" 'living:"true"]{Prof. Leonard}
}]

In this form, each attribute key starts with a quote mark @litchar{'} and ends with a colon @litchar{:}. As before, the attribute value is in quotation marks.

Both of these forms will produce the same X-expression:

@repl-output{'(span ((class "author")(id "primary")(living "true")) "Prof. Leonard")}


Now that you know how to make tags and attributes, you might wonder whether Pollen markup can be used as a quick & dirty HTML-notation system. Sure — for a quick & dirty project, why not. Recall that @secref["X-expressions" #:doc '(lib "pollen/scribblings/pollen.scrbl")] are just alternative notation for the standard angle-bracket notation used in HTML. So if you wanted HTML like this:

@repl-output{<div class="red" style="font-size:150%">Important <em>News</em></div>}

You could write it in Pollen markup like so:

@repl-output{◊div['class:"red" style:"font-size:150%"]{Important ◊em{News}}}

And then just convert it (using the @racket[->html] function) into the HTML above. Thus, the tags you already know and love (?) can be used in Pollen markup, but with fewer keystrokes and cruft.

Still, if Pollen markup were just an alternative notation system for HTML tags, it would be pretty boring. As I alluded above, that's merely a boring way to use it. 

In the XML spirit, Pollen markup lets you use any tags you want. That's considerably less boring.

@subsection{What are custom tags good for?}

XML jocks can skip this section, since you already know. But if you've been mired in Markdown or HTML, read on.

Tags, broadly speaking, are a means of annotating a text with extra information, which I'll call @italic{metadata} (using that term in its generic sense, not in any fiddly computery way). Metadata is the key tool that enables an author to write a book with the benefits of @italic{semantic markup} and @italic{format independence}.

@subsection{Semantic markup}

@italic{Semantic markup} means adding metadata to text according to the meaning of the text, not merely its intended visual appearance. So rather than tagging @racketvalfont{RacketCon} with an @racketvalfont{em} tag, as we did above to indicate how the word should look, maybe we would tag it with an @racketvalfont{event} tag, to indicate what @italic{kind} of thing it is.

Semantic markup lets an author specify distinctions that would be ambiguous in pure visual terms, thereby capturing more meaning and intent. For instance, in books, italic styling is commonly applied to a number of unrelated types of information: emphasized words, movie titles, terms being used for the first time, headings, captions and labels, and so on. Under a non-semantic formatting scheme, perhaps one would tag them all @racketvalfont{em}. But in semantic terms, one would tag them @racketvalfont{movie-title}, @racketvalfont{first-use}, @racketvalfont{heading}, as appropriate.

This has two major benefits. First, by separating appearance and meaning, an author can manage the content of the book in useful ways. For instance, if every movie title were tagged as @racketvalfont{movie-title} rather than @racketvalfont{italic}, then it would be simple to generate a list of all movies mentioned in the book (for the author's benefit) or a page index of movie references (for the reader's benefit). But without that semantic tagging, a movie title couldn't be distinguished from any other italicized text.

@subsection{Format independence}

The second benefit of custom tags is @italic{format independence}, or the ability to change the rendering of the text to suit a particular device or context. 

 When a text is encrusted with format-specific visual tags — for instance, HTML tags — then the document markup is entangled with a single output format. If you only need one output format, fine.

 But increasingly, book authors have been called upon to publish their work in multiple formats: paper and PDF, but also web, e-book, or other natively digital formats, that connect to devices with differing display capabilities. 

 @margin-note{Yes, I know that many of these formats are based on variants of HTML. But the HTML you can use in a desktop web browser is quite different from, say, the HTML you can use in a Kindle .mobi file. The .mobi file has other technical requirements too, like an .ncx and .opf file. So despite some genetic kinship, these HTML-ish formats are best understood as separate targets.}

Using a display-driven model to manage this complexity is a terrible idea — as anyone who's tried it can attest. Converting from one display-based file type to another — for instance, word processor to HTML, or HTML to PDF — is an exercise in frustration and drain-circling expectations. 

This isn't surprising. For a long time, text processing has been dominated by this display-driven model. Most word processors, like Microsoft Word and Pages, have been built around this model. It worked well enough in the era where most documents were eventually going to be printed on paper (or a paper simulator like PDF). HTML was a technical leap forward, but not a conceptual leap: it mostly represented the display options available in a web browser.

@margin-note{There's a couple TeX fans at the back of the room, waving their arms. Yes, TeX got a lot of things right. In practice, however, it never became a core tool for electronic publishing (which, to be fair, didn't exist when TeX was written). Plenty of ideas in Pollen were lifted from TeX.}

For a document to be format independent, two conditions have to be satisfied.

First, the document has to be readable by other programs, so they can handle the conversion of format-independent markup into a format-specific rendering (e.g., mapping semantic tags like @racketvalfont{movie-title} onto visual tags like @racketvalfont{em}). Most word-processor formats, like Word's .docx, are bad for authoring because these formats are opaque and proprietary. We needn't get into the political objections. As a practical matter, they're inarguably restrictive — if you can't get your data out of your file, you're stuck.

Second, the document itself has to be represented in a way that's independent of the particularities of any one format. For instance, HTML is a bad authoring format because it encourages authors to litter their text with HTML-isms like @racketvalfont{h1} and @racketvalfont{span}. These have no meaning outside of HTML, and thus will always cause conversion problems. The @seclink["Prelude__my_principled_objection_to_Markdown"
         #:doc '(lib "pollen/scribblings/pollen.scrbl")]{same goes for Markdown}, which is simply HTML in disguise.



The solution to the first condition is to use text-based markup rather than proprietary file types. The solution to the second condition is to let authors define custom tags for the document, rather than the other way around. Pollen markup incorporates both of these ideas.


@subsection{Using custom tags}

You can insert a custom tag using the same syntax as any other tag. Suppose you want to use an @racketvalfont{event} tag to mark events. You would insert it like so:

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
I want to attend ◊event{RacketCon} this year.}]

This markup will turn into this X-expression:

@repl-output{'(root "I want to attend " (event "RacketCon") " this year.")}

Which is equivalent to this XML:

@repl-output{<root>I want to attend <event>RacketCon</event> this year.</root>}

In truth, Pollen doesn't notice any difference between a custom tag vs. a standard HTML tag vs. any other kind of tag. They're all just markup tags. If you want to restrict yourself to a certain vocabulary of tags, you can. If you want to set up Pollen to enforce those restrictions, you can do that too. But by default, Pollen doesn't impose restrictions like this. In general, you can pick any tag name you want, and it will work.

Don't take my word for it. See what happens if you write this:

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
I want to attend ◊verylongandimpracticaltagname{RacketCon} this year.}]

One small but important exception to this rule. If you were wondering why I sometimes call them @italic{tag functions} instead of just @italic{tags}, it's because under the hood, every tag is implemented as a function. The default behavior of this function is just to wrap the text in a tag with the given name. 

The benefit of treating tags as functions will become evident later in this tutorial. But the cost of this approach is that tags occupy the same namespace as the other functions available in Pollen (and by extension, Racket). So if you try to use a tag name that's already the name of an existing function, an error will occur. 

For instance, let's suppose you try to use a custom tag called  @racketvalfont{length}:

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
The Panama Canal is ◊length{77km} across.}]

When you run this file, you get an error:

@errorblock{length: contract violation;
expected: list?
  given: "77km"}

The problem is that Racket already provides a function called @racket[length]. Consistent with the usual rules of Pollen command notation, your command is interpreted as an attempt to invoke the @racket[length] function, rather than apply a tag named @racketvalfont{length}.

In practice, namespace clashes are rare. But if necessary, they're easy to work around (for the simplest method, see @secref["Invoking_tag_functions"
         #:doc '(lib "pollen/scribblings/pollen.scrbl")]).


@subsection{Choosing custom tags}

You just saw that using custom tags is easy. Choosing custom tags, on the other hand, is less science than art. As the author, it's up to you. Some guidelines:

@itemlist[


@item{@bold{You're never doing it wrong.} I wanted to make sure you knew the case for semantic markup. But if your life would be easier just using HTML tags directly, go ahead.}

@item{@bold{Tag iteratively.} Don't worry about getting all your tags right the first time through. Just as you write and then rewrite, add the tags that seem right now, and change or augment them later, because …}

@item{@bold{Tags emerge from writing.} It's hopeless to try to specify all your tags in advance. As you write, you'll learn things about the text, which will suggest new tags.}

@item{@bold{The best tag system is the one you'll stick with.} Tags aren't free. It takes effort to insert them consistently. Don't bother with an overambitious tag scheme that bores you more than it helps.}

@item{@bold{For boilerplate, tags are faster than text.} If you find yourself repeatedly formatting certain text in a certain way — for instance, lists and tables — extract the content and wrap it in a tag that encapsulates the boilerplate.}

]

And most important:

@itemlist[


@item{@bold{Tags are functions.} As I @seclink["Tags___tag_functions"
         #:doc '(lib "pollen/scribblings/pollen.scrbl")]{mentioned above}, every tag has a function behind it that uses the content of the tag as input. The default tag function just outputs the tag and its content.  But you can replace this with any kind of function. So in practice, you can offload a lot of labor to tags. }
]



 As we'll see in the next section, this is where your book truly becomes programmable.

@section{Tags are functions}

@(noskip-note)

If you've used HTML or XML, tags are just tags: things you type into the document that look the same going out as they did going in. Tags can be used to select document elements or assign styling (via CSS). But they don't have any deeper effect on the document content.

That's not so in Pollen. Under the hood, Pollen is just an alternate way of writing code in the Racket programming language. And tags, instead of being inert markers, are actually functions.

I think most of you know what a function is, but just to be safe — in programming, a @italic{function} is a chunk of code that accepts some input, processes it, and then returns a value. Asking a function to process some data is known as @italic{calling} the function. 

Leading us to the Three Golden Rules of Pollen Tags:

@itemlist[#:style 'ordered


@item{@bold{Every Pollen tag calls a function with the same name.}}

@item{@bold{The input values for that function are the attributes and content of the tag.}}

@item{@bold{The whole tag — tag name, attributes, and content — is replaced with the return value of the called function.}}

]

You've already seen the simplest kind of function in a Pollen document: the @seclink["Tags___tag_functions"
         #:doc '(lib "pollen/scribblings/pollen.scrbl")]{default tag function}, which emulates the behavior of standard markup tags. 

 Let's revisit an earlier example, now with the help of the Golden Rules:


@fileblock["article.html.pm" @codeblock{
#lang pollen
 
I want to attend ◊em{RacketCon ◊strong{this} year}.}]

What happens when you run this source? Working from the inside out, Pollen calls the function @racketvalfont{strong} with the input @racketvalfont{"this"}. The result is @racketvalfont{(strong "this")}. Then Pollen calls the function @racketvalfont{em} with the three input values @racketvalfont{"RacketCon " (strong "this") " year"}, which yields @racketvalfont{(em "RacketCon " (strong "this") " year")}. Finally, Pollen calls the @racketvalfont{root} function with everything in the document, resulting in:

@repl-output{'(root "I want to attend " (em "RacketCon " (strong "this") " year") ".")}

@subsection{Attaching behavior to tags}

Sometimes this default behavior will suffice. But other times, you'll want to change the behavior of a tag. Why? Here are some useful examples of what you, as an author, can do with custom tag functions:

@itemlist[

@item{Automatically detect cross-references and add hyperlinks.}

@item{Pull in data from an external source.}

@item{Generate tables, figures, and other fiddly layout objects.}

@item{Change content based on given conditions.}

@item{Automatically detect line breaks, paragraphs, and lists.}

@item{Insert boilerplate text.}

@item{Anything annoying or repetitive.}

@item{Mathematical computations.}

@item{… and anything else you like to do with a programming language.}
]

@margin-note{Having invited you to gaze across these vistas, I apologize that my example here in this tutorial is necessarily tip-of-the-iceberg. I'll be adding a more detailed guide to writing Pollen functions, both simple and crafty.}

How do you change the behavior of a tag? By 1) writing a new function and 2) giving it the name of the tag. Once you do this, this new behavior will automatically be invoked when you use the tag.

For example, let's redefine the @racketvalfont{strong} tag in our example above to simply print @racketvalfont{BOOM}:

@fileblock["article.html.pm" @codeblock{
#lang pollen

◊define[(strong . lines)]{BOOM}

I want to attend ◊em{RacketCon ◊strong{this} year}}]


When you run this file, you indeed get:

@repl-output{'(root "I want to attend " (em "RacketCon " "BOOM" " year"))}

How does this work? First, although you can define a function in Pollen command syntax using either of @secref["The_two_command_modes__text_mode___Racket_mode"
         #:doc '(lib "pollen/scribblings/pollen.scrbl")], it tends to be easier to use Racket mode. I wrote the first one in text mode. But for clarity, I'm going to switch to Racket mode (run this file and convince yourself it comes out the same):

@fileblock["article.html.pm" @codeblock{
#lang pollen

◊(define (strong word) "BOOM")

I want to attend ◊em{RacketCon ◊strong{this} year}.}]

Let's look at our new function definition. As usual, we start with the lozenge character (@litchar{◊}) to denote a Pollen command. Then we use @racket[define] to introduce a function definition. The name of the function comes next, which needs to match our tag name, @racketvalfont{strong}. The expression @racket[(strong word)] means ``the name of this function is @racket[strong], and it takes a single word as input, which we'll refer to as @racket[word].'' Finally we have the return value, which is @racket["BOOM"].

Let's run this file again, but go back to the Golden Rules to understand what happens. Working from the inside out, Pollen calls the function @racketvalfont{strong} with the input @racketvalfont{"this"} — same as before. But this time, the result of the @racket[strong] function is not @racketvalfont{(strong "this")}, but simply @racketvalfont{BOOM}. Then Pollen calls the function @racketvalfont{em} with the three input values @racketvalfont{"RacketCon " "BOOM" " year"}, which yields @racketvalfont{(em "RacketCon " "BOOM" " year")}. Finally, Pollen calls the @racketvalfont{root} function with everything in the document, resulting in:

@repl-output{'(root "I want to attend " (em "RacketCon " "BOOM" " year"))}

This example is contrived, of course. But the basic idea — defining a function with the name of a tag — is the foundation of programmability in Pollen. @bold{If you get this, and the Golden Rules, you get everything.}

@subsection{Notes for experienced programmers}

Having said that, some of you are probably eager to hack around a bit. Let me chip off a few more cubes from the iceberg to help you on your way. (Everyone else, take five.)

@subsubsection{Point of no @code{return}} If you've written functions in other programming languages, you might be accustomed to using a @code{return} statement to send a value back from the function. This doesn't exist in Pollen or Racket — the return value of any function is just the last expression evaluated. In the example below, @racketvalfont{"BAP"} becomes the return value because it's in the last position, and @racketvalfont{"BOOM"} is ignored:

@fileblock["article.html.pm" @codeblock{
#lang pollen

◊(define (strong word) "BOOM" "BAP")

I want to attend ◊em{RacketCon ◊strong{this} year}.}]

@subsubsection{Multiple input values & rest arguments} Sometimes a tag will have only one word or string that becomes its input. More likely, however, it will have multiple values (this is inevitable with nested tags, because the results aren't concatenated). For instance, if we attach our function to @racket[em] rather than @racket[strong]:

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

The error arises because the @racket[em] function is getting three arguments — @racketvalfont{"RacketCon " "BOOM" " year"} — but has been defined to only accept one argument, @racket[word]. This is the ``arity mismatch.''

To fix this, it's better to get in the habit of writing tag functions that accept an indefinite number of input values. You do this by defining your function with a @italic{@seclink["contracts-rest-args" #:doc '(lib "scribblings/guide/guide.scrbl")]{rest argument}} (as in, ``give me the rest of the input values.'') To use a rest argument, put it last in your list of input arguments, and add a period @litchar{.} before:

@fileblock["article.html.pm" @codeblock{
#lang pollen

◊(define (em . parts) "BOOM")

I want to attend ◊em{RacketCon ◊strong{this} year}.}]

This time, the source file will run without an error, producing this:

@repl-output{'(root "I want to attend " "BOOM" ".")}

A rest argument like @racket[parts] is a @racket[list] of individual arguments. So if you want to unpack & process these arguments separately, you can use Racket's extensive list-processing functions (see @secref["pairs" #:doc '(lib "scribblings/guide/guide.scrbl")]). Also see @racket[quasiquote] below.

@subsubsection{Returning an X-expression} Often, you won't use a tag function to replace a whole tag with a string — you'll replace it with a different tag, described by an X-expression, like so:

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

@subsubsection{Interpolating variables into strings} The usual way is to use the @racket[format] function:

@racket[(format "String with variable: ~a" variable-name)]

See the docs for @racket[format] and @racket[fprintf] for your options.

Be careful if you're working with integers and X-expressions — a raw integer is treated as a character code, not an integer string. Using @racket[format] is essential:

@examples[#:eval my-eval
(->html '(div "A raw integer indicates a character code: " 42))
(->html `(div "Use format to make it a string: " ,(format "~a" 42)))
]

@subsubsection{Parsing attributes}

Detecting attributes in an argument list can be tricky because a) the tag may or may not have attributes, b) those attributes may be in standard or abbreviated syntax. For this reason, Pollen provides a @racket[split-attributes] function (in the @racket[pollen/tag] librar) that you can use in custom tag functions to separate the attributes and elements:

@fileblock["article.html.pm" @codeblock{
#lang pollen

◊(require pollen/tag)

◊(define (em . parts) 
  (define-values (attributes elements) (split-attributes parts))
  `(extra ,attributes (big ,@"@"elements)))

I want to attend ◊em['key: "value"]{RacketCon}.}]

This will move the @racket[elements] inside the @racket[big] tag, and attach the @racket[attributes] to the @racket[extra] tag:

@repl-output{'(root "I want to attend " (extra ((key "value")) (big "RacketCon")) ".")}


@section[#:tag-prefix "tutorial-3"]{Intermission}

That was a lot of heavy material. But it also covered the most essential idea in Pollen: that @bold{every tag is a function}. Congratulations on making it this far. 

The good news is that the rest of this tutorial will feel more relaxed, as we put these new principles to work. 

Sorry that this tutorial is longer than the others, but truly — this is the stuff that makes Pollen different. If you're not feeling enthusiastic by now, you should @link["http://www.buzzfeed.com/search?q=puppies"]{bail out}. 

Otherwise, get ready to rock.

@section{Organizing functions}

In the tag-function examples so far, we've defined each function within the source file where we used it. This is fine for quick little functions. 

But more often, you're going to want to use functions defined elsewhere, and store your own functions available so they're available to your source files.

@margin-note{For now, we're just invoking functions within a Pollen markup file. But as you'll see in the fourth tutorial, any function can be called from any kind of Pollen source file.}

@subsection{Using Racket's function libraries}

Any function in Racket's extensive libraries can be called by loading the library with the @racket[require] command, which will make all its functions and constants available with the usual Pollen command syntax:

@fileblock["article.html.pm" @codeblock{
#lang pollen

◊(require racket/math)

Pi is close to ◊|pi|.
The hyperbolic sine of pi is close to ◊(sinh pi).
}]

The result:

@repl-output{'(root "Pi is close to " 3.141592653589793 "." "\n" "The hyperbolic sine of pi is close to " 11.548739357257748 ".")}

One caveat — you're still in a Pollen markup file, so the return value of whatever function you call has to produce a string or an X-expression, so it can be merged into the document. @margin-note*{This is similar to the restriction introduced in the @seclink["Setting_up_a_preprocessor_source_file"
         #:doc '(lib "pollen/scribblings/pollen.scrbl")]{first tutorial} where functions used in preprocessor files had to produce text.}
Pollen won't stop you from calling a function that returns an incompatible value, like @racket[plot], which returns a bitmap image: 

@fileblock["article.html.pm" @codeblock{
#lang pollen

◊(require math plot)

Here's a sine wave:
◊(plot (function sin (- pi) pi #:label "y = sin(x)"))
}]

But it won't work when you try to run it in DrRacket or load it in the project server.

It would be fine, however, to call a different kind of @racket[plot] function that returned an SVG result, because any XML-ish data structure can be converted to an X-expression. 

@margin-note{Super web nerds also know that binary data can be converted into XML-ish form by encoding the file as a base-64 data URL — but if you know what I'm talking about, then you don't need my help to try it.}

For functions that don't return a string or an X-expression, you can always make a conversion by hand. For instance, consider @racket[range], a Racket function that returns a list of integers:

@fileblock["article.html.pm" @codeblock{
#lang pollen

◊(require racket/list)
A list of integers: ◊(range 5)
}]

This will produce an error in DrRacket:

@errorblock{
pollen markup error: in '(root "A list of integers: " (0 1 2 3 4)), '(0 1 2 3 4) is not a valid element (must be txexpr, string, symbol, XML char, or cdata)
}

In a case like this, you can explicitly convert the return value to a string (in whatever way makes sense):

@fileblock["article.html.pm" @codeblock{
#lang pollen

◊(require racket/list racket/string)
A list of integers: ◊(string-join (map number->string (range 5)))
}]

And get this output:

@repl-output{'(root "A list of integers: " "0 1 2 3 4")}


@subsection[#:tag-prefix "tutorial-3"]{The @racketfont{directory-require.rkt} file}

@(noskip-note)

As you get more comfortable attaching behavior to tags using tag functions, you'll likely want to create some functions that can be shared between multiple source files. The @racketfont{directory-require.rkt} file is a special file that is automatically imported by Pollen source files in the same directory. So every function and value provided by @racketfont{directory-require.rkt} can be used in these Pollen files.

First, using this file is not mandatory. You can always import functions and values from another file using @racket[require] (as seen in the previous section). The @racketfont{directory-require.rkt} is just meant to cure the tedium of importing the same file into every Pollen source file in your project. In a small project, not much tedium; in a large project, more.

Second, notice from the @racketfont{.rkt} suffix that @racketfont{directory-require.rkt} is a source file containing Racket code, not Pollen code. This is the default because while Pollen is better for text-driven source files, Racket is better for code-driven source files. Still, the choice is yours: the name of this file can be changed by resetting the @racket[world:directory-require] value.

Third, notice from the @racketfont{directory-} prefix that @racketfont{directory-require.rkt} is only used by Pollen source files @italic{in the same directory}. So if your project has source files nested inside a subdirectory, you'll need to explicitly create another @racketfont{directory-require.rkt} there and share the functions & values as needed.

@margin-note{``Why not make this file visible throughout a project, rather than just a directory?'' Good idea, but I couldn't figure out how to do it without creating finicky new dependencies. If you have a better idea, I'm open to it.}

Let's see how this works in practice. In the same directory as @racketfont{article.html.pm}, create a new @racketfont{directory-require.rkt} file as follows:

@fileblock["directory-require.rkt" @codeblock{
#lang racket
(define author "Trevor Goodchild")
(provide author)
}]

Here we use the @racket[define] function (which we've seen before) to set @racket[author] equal to @racket["Trevor Goodchild"]. Note the final step: consistent with standard Racket rules, we have to explicitly @racket[provide] the new value so that other files can see it (unlike Python, things you @racket[define] in Racket are private by default, not public).

Then update good old @racketfont{article.html.pm}:

@fileblock["article.html.pm" @codeblock{
#lang pollen

The author is ◊|author|.
}]

Run this in DrRacket and you'll get:

@repl-output{'(root "The author is " "Trevor Goodchild" ".")}

Now, in the same dirctory, create a second Pollen source file:

@fileblock["barticle.html.pm" @codeblock{
#lang pollen

The author is really ◊|author|?
}]

Run this, and you'll get:

@repl-output{'(root "The author is really " "Trevor Goodchild" "?")}

That's all there is to it. Everything provided by @racketfont{directory-require.rkt} is automatically available within each Pollen source file.

You can include functions, including tag functions, the same way. For instance, add a function for @racket[em]:

@fileblock["directory-require.rkt" @codeblock{
#lang racket
(define author "Trevor Goodchild")
(define (em . parts) `(extra (big ,@"@"parts)))
(provide author em)
}]

Then use it in a source file:

@fileblock["article.html.pm" @codeblock{
#lang pollen

The ◊em{author} is ◊em{◊|author|}.
}]

With the expected results:

@repl-output{'(root "The " (extra (big "author")) " is " (extra (big "Trevor Goodchild")) ".")}

@;subsection{Importing from a Pollen source file}

@;subsection{Making a Racket package}


@section{Putting it all together}

[Coming soon]

@;section{
1 directory-require.rkt
2-4 three source files
5 pagetree
6 template
}
