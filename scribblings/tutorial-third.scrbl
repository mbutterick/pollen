#lang scribble/manual

@(require scribble/eval (for-label pollen/decode plot pollen/world pollen/tag racket/base pollen/template txexpr racket/list racket/string))
@(require "mb-tools.rkt")

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/decode pollen/template pollen/tag xml racket/list txexpr))


@title[#:tag "third-tutorial"]{Third tutorial: Pollen markup & tag functions}

Now you're getting to the good stuff. In this tutorial, you'll use Pollen to publish a multi-page article written in Pollen markup. You'll learn about:

@itemlist[


@item{Adding tags & attributes with Pollen markup}

@item{Attaching behavior to tag functions}

@item{the @filepath{pollen.rkt} file}

@item{Using @racket[decode] with Pollen markup}

@;item{@exec{raco pollen render} and @exec{raco pollen publish}}


]

If you want the shortest possible introduction to Pollen, try the @secref["quick-tour"].

@section[#:tag-prefix "tutorial-3"]{Prerequisites}

I'll assume you've completed the @seclink["second-tutorial"]{second tutorial} and that you understand the principles of Pollen authoring mode — creating source files, converting them to X-expressions, and then combining them with templates to make output files. 

Because now it's time to pick up the pace. You've learned how to do some handy things with Pollen. But we haven't yet exploited the full fusion of writing environment and programming language. I promised you that @secref["the-book-is-a-program"], right? So let's do some programming.


@section{Pollen markup vs. XML}

You can skip this section if XML holds no interest. But Pollen markup evolved out of my attempt to come up with an alternative to XML that would be more usable for writing. So if you're familiar with XML, the contrast may be helpful. 

@subsection{The XML problem}

In the @seclink["second-tutorial"]{second tutorial}, I made the case that Markdown is a limiting format for authors. Why? Markdown is essentially a notation system for HTML tags. As such, it has three problems: it's not semantic, it only covers a limited subset of HTML tags, and it can't be extended by an author. 

These problems are partly limitations of HTML itself. And these limitations were meant to be cured by XML — the @italic{X} stands for @italic{extensible}. In principle, XML allows you to define whatever tags you like and use them in your document.

So why hasn't XML taken over the world? In practice, XML promises more than it delivers. The reasons are apparent to any writer who's attempted to use XML as an authoring format:

@itemlist[

@item{@bold{Verbose syntax}. Unfortunately, XML relies on the same angle-bracket notation as HTML. If you think HTML source is hard to read, XML is worse. Since much of writing involves reading, this feature is also a major bug.}

@item{@bold{Validation overhead}. Integral to XML is the concept of @defterm{validation}, which guarantees that a document meets certain formal criteria, usually asserted in a @italic{schema}. To get the full value from XML, you generally want to use validation. But doing so imposes a lot more work on you as an author, and removes much of the expressive potential of XML.}

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

Pollen markup is a free-form markup system that lets you add arbitrary @defterm{tags} and @defterm{attributes} to your text. By arbitrary, I mean that they don't need to match up with an existing schema or specification (e.g., the tags permitted by HTML). They can — but that's an option, not a requirement. 

I like to think of Pollen markup a way of capturing not just the text, but also my @bold{ideas about the text}. Some of these are low-level ideas (``this text should be italicized''). Some are high-level ideas (``this text is the topic of the page''). Some are just notes to myself. In short, everything I know about the text becomes part of the text. 

In so doing, Pollen markup becomes the source code of the book. Let's try it out.

@subsection{Creating a Pollen markup file}

We're going to use Pollen markup to make a file that will ultimately be HTML. So consistent with the authoring-mode workflow we learned in the @seclink["second-tutorial"]{second tutorial}, we'll start with our desired output filename, @filepath{article.html}, and then append the Pollen markup suffix, @filepath{.pm}.

In DrRacket, start a new file called @filepath{article.html.pm} like so (BTW you can use any sample text you like):

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
I want to attend RacketCon this year.
}]

Consistent with usual authoring-mode policy, when you run this file, you'll get an X-expression that starts with @code{root}:

@repl-output{'(root "I want to attend RacketCon this year.")}

Remember, even though the first line of the file is @racketmodfont{#lang} @racketmodname[pollen] — same as the last tutorial — the new @filepath{.pm} suffix signals that Pollen should interpret the source as Pollen markup. Look what happens if you goof up and put Markdown source in a Pollen markup file, like so:

@codeblock{
#lang pollen
 
I am **so** excited to attend __RacketCon__ this year.
}

The Markdown syntax will be ignored, and pass through to the output:

@repl-output{'(root "I am **so** excited to attend __RacketCon__ this year.")}

Restore the non-Markdown source, and let's continue.


@subsection{Tags & tag functions}

Pollen markup uses the same Pollen command syntax that we first saw in @secref["Adding_commands"]. Previously, we used this command syntax to invoke functions like @racket[define] and @racket[->html]. Pollen markup is used to invoke a special kind of function called a @defterm{tag function}, which is a function that, by default, adds a tag to the text.

To see how this works, restore your @filepath{article.html.pm} file to its original state:

@codeblock{
#lang pollen
 
I want to attend RacketCon this year.
}

We can add any tag with Pollen markup, but for now, let's start with an old favorite: @code{em}, which is used in HTML to add emphasis to text. We apply a tag by starting with the lozenge character (◊) followed by the tag name @code{em}, followed by the text in curly braces, like so:

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

@defterm{Attributes} are like tags for tags. Each attribute is a key–value pair where the key is any name, and the value is a string. Anyone who's seen HTML is familiar with them:

@repl-output{<span class="author">Prof. Leonard</span>}

Here, @code{class} is an attribute for @code{span} that has value @code{"author"}. And this is what it looks like as an X-expression:

@repl-output{'(span ((class "author")) "Prof. Leonard")}

You can add any number of attributes to a tag (first as an X-expression, then as HTML):

@repl-output{'(span ((class "author")(id "primary")(living "true")) "Prof. Leonard")}

@repl-output{<span class="author" id="primary" living="true">Prof. Leonard</span>}

In Pollen markup, attributes have the same logic, but a slightly different syntax. In keeping with the tag notation you just saw, the @code{span} tag is added in the usual way:

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


Now that you know how to make tags and attributes, you might wonder whether Pollen markup can be used as a quick & dirty HTML-notation system. Sure — for a quick & dirty project, why not. Recall that @secref["X-expressions"] are just alternative notation for the standard angle-bracket notation used in HTML. So if you wanted HTML like this:

@repl-output{<div class="red" style="font-size:150%">Important <em>News</em></div>}

You could write it in Pollen markup like so:

@repl-output{◊div['class:"red" style:"font-size:150%"]{Important ◊em{News}}}

And then just convert it (using the @racket[->html] function) into the HTML above. Thus, the tags you already know (and love?) can be used in Pollen markup, but with fewer keystrokes and cruft.

Still, if Pollen markup were just an alternative notation system for HTML tags, it would be pretty boring. As I alluded above, that's merely a boring way to use it. 

In the XML spirit, Pollen markup lets you use any tags you want. That's considerably less boring.

@subsection{What are custom tags good for?}

XML jocks can skip this section, since you already know. But if you've been mired in Markdown or HTML, read on.

Tags, broadly speaking, are a means of annotating a text with extra information, which I'll call @defterm{metadata} (using that term in its generic sense, not in any fiddly computery way). Metadata is the key tool that enables an author to write a book with the benefits of @defterm{semantic markup} and @defterm{format independence}.

@subsection{Semantic markup}

@defterm{Semantic markup} means adding metadata to text according to the meaning of the text, not merely its intended visual appearance. So rather than tagging @code{RacketCon} with an @code{em} tag, as we did above to indicate how the word should look, maybe we would tag it with an @code{event} tag, to indicate what @italic{kind} of thing it is.

Semantic markup lets an author specify distinctions that would be ambiguous in pure visual terms, thereby capturing more meaning and intent. For instance, in books, italic styling is commonly applied to a number of unrelated types of information: emphasized words, movie titles, terms being used for the first time, headings, captions and labels, and so on. Under a non-semantic formatting scheme, perhaps one would tag them all @code{em}. But in semantic terms, one would tag them @code{movie-title}, @code{first-use}, @code{heading}, as appropriate.

This has two major benefits. First, by separating appearance and meaning, an author can manage the content of the book in useful ways. For instance, if every movie title were tagged as @code{movie-title} rather than @code{italic}, then it would be simple to generate a list of all movies mentioned in the book (for the author's benefit) or a page index of movie references (for the reader's benefit). But without that semantic tagging, a movie title couldn't be distinguished from any other italicized text.

@subsection{Format independence}

The second benefit of custom tags is @defterm{format independence}, or the ability to change the rendering of the text to suit a particular device or context. 

 When a text is encrusted with format-specific visual tags — for instance, HTML tags — then the document markup is entangled with a single output format. If you only need one output format, fine.

 But increasingly, book authors have been called upon to publish their work in multiple formats: paper and PDF, but also web, e-book, or other natively digital formats, that connect to devices with differing display capabilities. 

 @margin-note{Yes, I know that many of these formats are based on variants of HTML. But the HTML you can use in a desktop web browser is quite different from, say, the HTML you can use in a Kindle @code{.mobi} file. The @code{.mobi} file has other technical requirements too, like an @code{.ncx} and @code{.opf} file. So despite some genetic kinship, these HTML-ish formats are best understood as separate targets.}

Using a display-driven model to manage this complexity is a terrible idea — as anyone who's tried it can attest. Converting from one display-based file type to another — for instance, word processor to HTML, or HTML to PDF — is an exercise in frustration and drain-circling expectations. 

This isn't surprising. For a long time, text processing has been dominated by this display-driven model. Most word processors, like Microsoft Word and Pages, have been built around this model. It worked well enough in the era where most documents were eventually going to be printed on paper (or a paper simulator like PDF). HTML was a technical leap forward, but not a conceptual leap: it mostly represented the display options available in a web browser.

@margin-note{There's a couple TeX fans at the back of the room, waving their arms. Yes, TeX got a lot of things right. In practice, however, it never became a core tool for electronic publishing (which, to be fair, didn't exist when TeX was written). Plenty of ideas in Pollen were lifted from TeX.}

For a document to be format independent, two conditions have to be satisfied.

First, the document has to be readable by other programs, so they can handle the conversion of format-independent markup into a format-specific rendering (e.g., mapping semantic tags like @code{movie-title} onto visual tags like @code{em}). Most word-processor formats, like Word's .docx, are bad for authoring because these formats are opaque and proprietary. We needn't get into the political objections. As a practical matter, they're inarguably restrictive — if you can't get your data out of your file, you're stuck.

Second, the document itself has to be represented in a way that's independent of the particularities of any one format. For instance, HTML is a bad authoring format because it encourages authors to litter their text with HTML-isms like @code{h1} and @code{span}. These have no meaning outside of HTML, and thus will always cause conversion problems. The @seclink["The_case_against_Markdown"]{same goes for Markdown}, which is simply HTML in disguise.



The solution to the first condition is to use text-based markup rather than proprietary file types. The solution to the second condition is to let authors define custom tags for the document, rather than the other way around. Pollen markup incorporates both of these ideas.


@subsection{Using custom tags}

You can insert a custom tag using the same syntax as any other tag. Suppose you want to use an @code{event} tag to mark events. You would insert it like so:

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

One small but important exception to this rule. If you were wondering why I sometimes call them @defterm{tag functions} instead of just @defterm{tags}, it's because under the hood, every tag is implemented as a function. The default behavior of this function is just to wrap the text in a tag with the given name. 

The benefit of treating tags as functions will become evident later in this tutorial. But the cost of this approach is that tags occupy the same namespace as the other functions available in Pollen (and by extension, Racket). So if you try to use a tag name that's already the name of an existing function, an error will occur. 

For instance, let's suppose you try to use a custom tag called  @code{length}:

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
The Panama Canal is ◊length{77km} across.}]

When you run this file, you get an error:

@errorblock{length: contract violation;
expected: list?
  given: "77km"}

The problem is that Racket already provides a function called @racket[length]. Consistent with the usual rules of Pollen command notation, your command is interpreted as an attempt to invoke the @racket[length] function, rather than apply a tag named @code{length}.

In practice, namespace clashes are rare. But if necessary, they're easy to work around (for the simplest method, see @secref["Invoking_tag_functions"]).


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


@item{@bold{Tags are functions.} As I @seclink["Tags___tag_functions"]{mentioned above}, every tag has a function behind it that uses the content of the tag as input. The default tag function just outputs the tag and its content.  But you can replace this with any kind of function. So in practice, you can offload a lot of labor to tags. }
]



 As we'll see in the next section, this is where your book truly becomes programmable.

@section[#:tag "tags-are-functions"]{Tags are functions}

@(noskip-note)

If you've used HTML or XML, tags are just tags: things you type into the document that look the same going out as they did going in. Tags can be used to select document elements or assign styling (via CSS). But they don't have any deeper effect on the document content.

That's not so in Pollen. Under the hood, Pollen is just an alternate way of writing code in the Racket programming language. And tags, instead of being inert markers, are actually functions.

I think most of you know what a function is, but just to be safe — in programming, a @defterm{function} is a chunk of code that accepts some input, processes it, and then returns a value. Asking a function to process some data is known as @defterm{calling} the function. 

Leading us to the Three Golden Rules of Pollen Tags:

@itemlist[#:style 'ordered


@item{@bold{Every Pollen tag calls a function with the same name.}}

@item{@bold{The input values for that function are the attributes and content of the tag.}}

@item{@bold{The whole tag — tag name, attributes, and content — is replaced with the return value of the called function.}}

]

Corollary to rule #3: because a tag represents a single X-expression, a tag function must also return a single X-expression. If you want to return multiple elements, you'll need to wrap them in a new tag (thus grouping them into a single X-expression). (If you absolutely must splice multiple elements in place of a single X-expression, you'll need to use @racket[decode] rather than a tag function.)

You've already seen the simplest kind of function in a Pollen document: the @seclink["Tags___tag_functions"]{default tag function}, which emulates the behavior of standard markup tags. 

 Let's revisit an earlier example, now with the help of the Golden Rules:


@fileblock["article.html.pm" @codeblock{
#lang pollen
 
I want to attend ◊em{RacketCon ◊strong{this} year}.}]

What happens when you run this source? Working from the inside out, Pollen calls the function @code{strong} with the input @code{"this"}. The result is @code{(strong "this")}. Then Pollen calls the function @code{em} with the three input values @code{"RacketCon " (strong "this") " year"}, which yields @code{(em "RacketCon " (strong "this") " year")}. Finally, Pollen calls the @code{root} function with everything in the document, resulting in:

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


How do you change the behavior of a tag? By 1) writing a new function and 2) giving it the name of the tag. Once you do this, this new behavior will automatically be invoked when you use the tag.

For example, let's redefine the @code{strong} tag in our example above to simply print @code{BOOM}:

@fileblock["article.html.pm" @codeblock{
#lang pollen

◊define[(strong . lines)]{BOOM}

I want to attend ◊em{RacketCon ◊strong{this} year}}]


When you run this file, you indeed get:

@repl-output{'(root "I want to attend " (em "RacketCon " "BOOM" " year"))}

How does this work? First, although you can define a function using either of @secref["the-two-command-modes"], it tends to be easier to use Racket mode. I wrote the first one in Pollen mode. But for clarity, I'm going to switch to Racket mode (run this file and convince yourself it comes out the same):

@fileblock["article.html.pm" @codeblock{
#lang pollen

◊(define (strong word) "BOOM")

I want to attend ◊em{RacketCon ◊strong{this} year}.}]

Let's look at our new function definition. As usual, we start with the lozenge character (@litchar{◊}) to denote a Pollen command. Then we use @racket[define] to introduce a function definition. The name of the function comes next, which needs to match our tag name, @code{strong}. The expression @racket[(strong word)] means ``the name of this function is @racket[strong], and it takes a single word as input, which we'll refer to as @racket[word].'' Finally we have the return value, which is @racket["BOOM"].

Let's run this file again, but go back to the Golden Rules to understand what happens. Working from the inside out, Pollen calls the function @code{strong} with the input @code{"this"} — same as before. But this time, the result of the @racket[strong] function is not @code{(strong "this")}, but simply @code{BOOM}. Then Pollen calls the function @code{em} with the three input values @code{"RacketCon " "BOOM" " year"}, which yields @code{(em "RacketCon " "BOOM" " year")}. Finally, Pollen calls the @code{root} function with everything in the document, resulting in:

@repl-output{'(root "I want to attend " (em "RacketCon " "BOOM" " year"))}

This example is contrived, of course. But the basic idea — defining a function with the name of a tag — is the foundation of programmability in Pollen. @bold{If you get this, and the Golden Rules, you get everything.}


@section[#:tag-prefix "tutorial-3"]{Intermission}

That was a lot of heavy material. But it also covered the most essential idea in Pollen: that @bold{every tag is a function}. Congratulations on making it this far. 

@margin-note{Experienced programmers might want to take a detour through @secref["programming-pollen"] to understand more about what's possible with tag functions.}

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

One caveat — you're still in a Pollen markup file, so the return value of whatever function you call has to produce a string or an X-expression, so it can be merged into the document. @margin-note*{This is similar to the restriction introduced in the @seclink["Setting_up_a_preprocessor_source_file"]{first tutorial} where functions used in preprocessor files had to produce text.}
Pollen won't stop you from calling a function that returns an incompatible value, like @racket[plot], which returns a bitmap image: 

@fileblock["article.html.pm" @codeblock{
#lang pollen

◊(require math plot)

Here's a sine wave:
◊(plot (function sin (- pi) pi #:label "y = sin(x)"))
}]

But it won't work when you try to run it in DrRacket or load it in the project server.

It would be fine, however, to call a different kind of @racket[plot] function that returned an SVG result, because any XML-ish data structure can be converted to an X-expression. 

@margin-note{Super web nerds also know that binary data can be converted into XML-ish form by encoding the file as a base64 data URL — but if you know what I'm talking about, then you don't need my help to try it.}

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


@subsection[#:tag-prefix "tutorial-3"]{Using the @filepath{pollen.rkt} file}

@(noskip-note)

As you get more comfortable attaching behavior to tags using tag functions, you'll likely want to create some functions that can be shared between multiple source files. The @filepath{pollen.rkt} file is a special file that is automatically imported by Pollen source files in the same directory (including within subdirectories). So every function and value provided by @filepath{pollen.rkt} can be used in these Pollen files.

First, using this file is not mandatory. You can always import functions and values from another file using @racket[require] (as seen in the previous section). The @filepath{pollen.rkt} is just meant to cure the tedium of importing the same file into every Pollen source file in your project. In a small project, not much tedium; in a large project, more.

Second, notice from the @filepath{.rkt} suffix that @filepath{pollen.rkt} is a source file containing Racket code, not Pollen code. This is the default because while Pollen is better for text-driven source files, Racket is better for code-driven source files.

Third, @filepath{pollen.rkt} always applies to Pollen source files in the same directory. But that's the minimum scope for the file, not the maximum. Pollen source files nested in subdirectories will look for a @filepath{pollen.rkt} in their own directory first. But if they can't find it, they'll look in the parent directory, then the next parent directory, and so on. Thus, by default, a @filepath{pollen.rkt} in the root folder of a project will apply to all the source files in the project. But when you add a new @filepath{pollen.rkt} to a subdirectory, it will apply to all files in that subdirectory and below.

@margin-note{Though a subdirectory-specific @filepath{pollen.rkt} will supersede the one in the enclosing directory, you can still use @racket[(require "../pollen.rkt")] to pull in definitions from above, and @racket[provide] to propagate them into the current subdirectory. For instance, @racket[(provide (all-from-out "../pollen.rkt"))] will re-export everything from the parent directory.}

Let's see how this works in practice. In the same directory as @filepath{article.html.pm}, create a new @filepath{pollen.rkt} file as follows:

@fileblock["pollen.rkt" @codeblock{
#lang racket
(define author "Trevor Goodchild")
(provide author)
}]

Here we use the @racket[define] function (which we've seen before) to set @racket[author] equal to @racket["Trevor Goodchild"]. Note the final step: consistent with standard Racket rules, we have to explicitly @racket[provide] the new value so that other files can see it (unlike Python, things you @racket[define] in Racket are private by default, not public).

Then update good old @filepath{article.html.pm}:

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

That's all there is to it. Everything provided by @filepath{pollen.rkt} is automatically available within each Pollen source file.

You can include functions, including tag functions, the same way. For instance, add a function for @racket[em]:

@fileblock["pollen.rkt" @codeblock{
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

@section{Decoding markup via the @tt{root} tag}

As you've seen, the X-expression you get when you run a Pollen markup file always starts with a  node called @code{root}. You can attach a tag function to @code{root} the same way as any other tag. For instance, you could do something simple, like change the name of the output X-expression:

@fileblock["article.html.pm" @codeblock|{
#lang pollen

◊(define (root . elements) `(content ,@elements))

The ◊code{root} tag is now called ◊code{content}.
}|]

Resulting in:

@repl-output{'(content "The " (tt "root") " tag is now called " (tt "content") ".")}

But unlike other tags in your document, @code{root} contains the entire content of the document. So the function you attach to @code{root} can operate on everything.

For that reason, one of the most useful things you can do with a tag function attached to @code{root} is @defterm{decoding} the content of the page. Decoding refers to any post-processing of content that happens after the tags within the page have been evaluated. 

Decoding is a good way to automatically accomplish:

@itemlist[

@item{Detection of linebreaks, paragraphs, and list items based on whitespace.}

@item{Hyphenation.}

@item{Typographic optimizations, like smart quotes, dashes, and ligatures.}

@item{Gathering data for indexing or cross-referencing.}

@item{Any document enhancements a) that can be handled programmatically and b) that you'd prefer not to hard-code within your source files.}

]

As an example, let's take one of my favorites — linebreak and paragraph detection. In XML authoring, you have to insert every @code{<br />} and @code{<p>} tag by hand. This is profoundly dull, clutters up the source file, and makes editing a chore. 

Instead, let's make a decoder that allows us to denote a linebreak with a single newline in the source, and a paragraph break with a double newline. Here's some sample content with single and double newlines:

@fileblock["article.html.pm" @codeblock|{
#lang pollen

The first line of the 'first' paragraph.
And a new line.

The second paragraph --- isn't it great.
}|]

Because we don't yet have a decoder, these newlines just get passed through:

@repl-output{'(root "The first line of the 'first' paragraph." "\n" "And a new line." "\n" "\n" "The second paragraph --- isn't it great.")}

When this X-expression is converted to HTML, the newlines persist:

@repl-output{<root>The first line of the 'first' paragraph.\nAnd a new line.\n\nThe second paragraph --- isn't it great.</root>}

But in HTML, raw newlines are displayed as a single space. So if you view this file in the project server, you'll see:

@browser{
The first line of the 'first' paragraph. And a new line. The second paragraph --- isn't it great.
}


Not what we want.

So we need to make a decoder. To do this, we use the @racket[decode-elements] function, which provides hooks to selectively process certain categories of content within the document. 

@margin-note{@racket[decode-elements] is a convenience variant of @racket[decode], which takes a full X-expression as input. Under the hood, they work the same way, so use whichever you prefer.}

Add a basic @racket[decode-elements] to the source file like so:

@fileblock["article.html.pm" @codeblock|{
#lang pollen

◊(require pollen/decode txexpr)
◊(define (root . elements)
   (make-txexpr 'root null (decode-elements elements)))

The first line of the 'first' paragraph.
And a new line.

The second paragraph --- isn't it great.
}|]

The @racket[make-txexpr] function is a utility from the @racket[txexpr] package, which is installed with Pollen. It builds a new X-expression from a tag, attribute list, and list of elements. Here, we'll keep the tag name @code{root}, leave the attributes as @code{null}, and append our decoded list of elements.

@margin-note{Racket jocks: you could also write this using @racket[quasiquote] and @racket[unquote-splicing] syntax as @code|{`(root ,@(decode-elements elements))}|. The @racket[txexpr] package is just an alternate way of accomplishing the task.}

If you run this file, what changes? Right — nothing. That's because by default, both @racket[decode-elements] (and @racket[decode]) will let the content pass through unaltered.

We change this by giving @racket[decode-elements] the name of a processing function and attaching it to the type of content we want to process. In this case, we're in luck — the @racket[decode] module already contains a @racket[detect-paragraphs] function (that also detects linebreaks). We add this function using the keyword argument @racket[#:txexpr-elements-proc], which is short for ``the function used to process the elements of a tagged X-expression'':

@fileblock["article.html.pm" @codeblock|{
#lang pollen

◊(require pollen/decode txexpr)
◊(define (root . elements)
   (make-txexpr 'root null (decode-elements elements
     #:txexpr-elements-proc detect-paragraphs)))

The first line of the 'first' paragraph.
And a new line.

The second paragraph --- isn't it great.
}|]

Now, when we run the file, the X-expression has changed to include two @racket[p] tags and a @racket[br] tag:

@repl-output{'(root (p "The first line of the 'first' paragraph." (br) "And a new line.") (p "The second paragraph --- isn't it great."))}

That means when we convert to HTML, we get the tags we need:

@repl-output{<root><p>The first line of the 'first' paragraph.<br />And a new line.</p><p>The second paragraph --- isn't it great.</p></root>}

So when we view this in the project server, the linebreaks and paragraph breaks are displayed correctly:

@browser{
The first line of the 'first' paragraph.
And a new line. 

The second paragraph --- isn't it great.
}


Of course, in practice you wouldn't put your decoding function in a single source file. You'd make it available to all your source files by putting it in @filepath{pollen.rkt}. So let's do that now:

@fileblock["pollen.rkt" @codeblock{
#lang racket
(require pollen/decode txexpr)
(define (root . elements)
   (make-txexpr 'root null (decode-elements elements
     #:txexpr-elements-proc detect-paragraphs)))
(provide root)
}]

We'll also restore the source of @filepath{article.html.pm} to its original, simplified state:

@fileblock["article.html.pm" @codeblock|{
#lang pollen

The first line of the 'first' paragraph.
And a new line.

The second paragraph --- isn't it great.
}|]

And the result in the project server will be the same:

@browser{
The first line of the 'first' paragraph.
And a new line. 

The second paragraph --- isn't it great.
}

But wait, those straight quotes look terrible. Also, three hyphens for an em dash? Barbaric. 

Let's upgrade our decoder to take of those. Once again, we'll get lucky, because the @racket[decode] module provides two functions for the job: @racket[smart-quotes] and @racket[smart-dashes].

This time, however, we're going to attach them to another part of @racket[decode-elements]. Smart-quote and smart-dash conversion only needs to look at the strings within the X-expression. So instead of attaching these functions to the @racket[#:txexpr-elements-proc] argument of @racket[decode-elements], we'll attach them to @racket[#:string-proc], which lets us specify a function to apply to strings:

@fileblock["pollen.rkt" @codeblock{
#lang racket/base
(require pollen/decode txexpr)
(define (root . elements)
   (make-txexpr 'root null (decode-elements elements
     #:txexpr-elements-proc detect-paragraphs
     #:string-proc (compose smart-quotes smart-dashes))))
(provide root)
}]

Because @racket[#:string-proc] only accepts one function (not two), we need to use @racket[compose] to combine @racket[smart-quotes] and @racket[smart-dashes] into one (@racket[compose], from the Racket library, will apply the last function, then the previous one, and so on to the left end of the list).

Now, if we run @filepath{article.html.pm} in DrRacket, we can see the effects of the new decoder functions. The quotes are curled, and the three hyphens become an em dash:

@repl-output{'(root (p "The first line of the ‘first’ paragraph." (br) "And a new line.") (p "The second paragraph—isn’t it great."))}

And of course, this shows up in the project server too:

@browser{
The first line of the ‘first’ paragraph.
And a new line.

The second paragraph—isn’t it great.    
}

By the way, even though decoding via the @code{root} tag is the most likely use case, you don't have to do it that way. Decoding is just a special kind of tag function. So you can make a decoder that only affects a certain tag within the page. Or you can make multiple decoders for different tags. The advantage of using a decoder with @code{root} is that it can affect all the content, and since it's attached to the root node, it will always be the last tag function that gets called.


@section{Putting it all together}

For this final example, we'll combine what we've learned in the first three tutorials. Though this project is still simple, it summarizes all the major concepts of Pollen. 

It also provides a recipe you can adapt for your own projects, whether small or large. For instance, @italic{@link["http://practicaltypography.com"]{Butterick's Practical Typography}} follows this core structure.

As we go through the ingredients, I'll review the purpose of each. Save these files into a single project directory with the project server running.

@subsection[#:tag-prefix "tutorial-3"]{The @filepath{pollen.rkt} file}

This file provides functions that are available to all Pollen source files in the same directory. It's written in standard Racket. The @filepath{pollen.rkt} file is optional — without it, your tags will just be treated as default tag functions. But you'll probably find it a convenient way to make tag functions available within your project, including a @racket[decode] function attached to @code{root}.

Here, we'll use the @filepath{pollen.rkt} we devised in the previous section to set up decoding for our source files:

@fileblock["pollen.rkt" @codeblock{
#lang racket/base
(require pollen/decode txexpr)
(define (root . elements)
   (make-txexpr 'root null (decode-elements elements
     #:txexpr-elements-proc detect-paragraphs
     #:string-proc (compose smart-quotes smart-dashes))))
(provide root)
}]


@subsection{The template}

When you're using Pollen authoring mode for your content — using either Markdown syntax, or Pollen markup — your source files will produce an X-expression. To convert this X-expression into a finished file, you need to use a template. 

By default, when Pollen finds a source file called @filepath{filename.ext.pm} or @filepath{filename.ext.pmd}, it will look for a template in your project directory called @filepath{template.ext}, where @filepath{.ext} is the matching output extension. 

In this project, we want to end up with HTML, so our source files will be called @filepath{filename.html.pm}, and thus we need to make a @filepath{template.html}. Let's use a modified version of the one we made in the second tutorial:

@fileblock["template.html"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
<meta charset="UTF-8">
<title>◊select['h1 doc] by T. S. Eliot</title>
<link rel="stylesheet" type="text/css" media="all" href="styles.css" />
</head>
<body>◊->html[doc]
◊(define prev-page (previous here))
◊when/block[prev-page]{
<div id="prev">← <a href="◊|prev-page|">◊(select 'h1 prev-page)</a></div>}
◊(define next-page (next here))
◊when/block[next-page]{
<div id="next"><a href="◊|next-page|">◊(select 'h1 next-page)</a> →</div>}
</body>
</html>
}]

@subsection{The pagetree}

A pagetree defines sequential and hierarchical relationships among a set of output files. The pagetree is used by the template to calculate navigational links (e.g., previous, next, up, etc.) A pagetree is optional — if you don't need navigation in your project, you don't need a pagetree.

But in this project, we do want navigation. So we'll add an @filepath{index.ptree} file like so:

@fileblock["index.ptree"
@codeblock{
#lang pollen

burial.html
chess.html
sermon.html
}]

@subsection{A CSS stylesheet using the preprocessor}

Our template file above refers to a CSS file called @filepath{styles.css}. When resolving linked files, the project server makes no distinction between static and dynamic files. If there's a static file called @filepath{styles.css}, it will use that. 

Or, if you make a preprocessor source file called @filepath{styles.css.pp}, it will be dynamically rendered into the requested @filepath{styles.css} file. The preprocessor will operate on any file with the @filepath{.pp} extension — so a preprocessor source called @filepath{filename.ext.pp} will be rendered into @filepath{filename.ext}. (The corollary is that preprocessor functionality can be added to @italic{any} kind of text-based file.)

Preprocessor source files, like authoring source files, get access to everything in @filepath{pollen.rkt}, so you can share common functions and variables.

Let's use an improved version of the dynamic CSS file we made in the first tutorial.

@fileblock["styles.css.pp"
@codeblock{
#lang pollen

◊(define inner 2)
◊(define edge (* inner 2))
◊(define color "gray")
◊(define multiplier 1.3)

body {
    margin: ◊|edge|em;
    border: ◊|inner|em double ◊|color|;
    padding: ◊|inner|em;
    font-size: ◊|multiplier|em;
    line-height: ◊|multiplier|;
}

h1 {
    font-size: ◊|multiplier|em;
}

#prev, #next {
    position: fixed;
    top: ◊|(/ edge 2)|em;
}

#prev {
    left: ◊|edge|em;
}

#next {
    right: ◊|edge|em;
}
}]

@subsection{The content source files using Pollen markup}

With the scaffolding in place, we need the content. Our pagetree contains three output files — @filepath{burial.html}, @filepath{chess.html}, and @filepath{sermon.html}. We're going to make these output files using Pollen markup. So we'll create three source files and name them by adding the @filepath{.pm} source extension to each of the output names — thus @filepath{burial.html.pm}, @filepath{chess.html.pm}, and @filepath{sermon.html.pm}, as follows (and with apologies to T. S. Eliot):


@fileblock["burial.html.pm" @codeblock[#:keep-lang-line? #f]{
#lang scribble/text
#lang pollen

◊h1{I. The Burial of the Dead}

"You gave me hyacinths first a year ago;
They called me the hyacinth girl."
--- Yet when we came back, late, from the Hyacinth garden,
Your arms full, and your hair wet, I could not
Speak, and my eyes failed, I was neither
Living nor dead, and I knew nothing,
Looking into the heart of light, the silence.
◊em{Od' und leer das Meer.}

Madame Sosostris, famous clairvoyante,
Had a bad cold, nevertheless
Is known to be the wisest woman in Europe,
With a wicked pack of cards.
}]

@fileblock["chess.html.pm" @codeblock[#:keep-lang-line? #f]{
#lang scribble/text
#lang pollen

◊h1{II. A Game of Chess}

And still she cried, and still the world pursues,    
"Jug Jug" to dirty ears.     
And other withered stumps of time    
Were told upon the walls; staring forms
Leaned out, leaning, hushing the room enclosed.  
Footsteps shuffled on the stair,     
Under the firelight, under the brush, her hair   
Spread out in fiery points   
Glowed into words, then would be savagely still.
 
"My nerves are bad to-night. Yes, bad. Stay with me.     
Speak to me. Why do you never speak? Speak.  
What are you thinking of? What thinking? What?   
I never know what you are thinking. Think."
}]

@fileblock["sermon.html.pm" @codeblock[#:keep-lang-line? #f]{
#lang scribble/text
#lang pollen

◊h1{III. The Fire Sermon}

"Trams and dusty trees.  
Highbury bore me. Richmond and Kew   
Undid me. By Richmond I raised my knees  
Supine on the floor of a narrow canoe." 
 
"My feet are at Moorgate, and my heart   
Under my feet. After the event   
He wept. He promised 'a new start.'  
I made no comment. What should I resent?"
}]

@subsection{The result}

Now visit the project server and view @filepath{burial.html}, which should look something like this (the box will expand to fit your browser window):

@image/rp["burial.png" #:scale 0.8]

Click the navigational links at the top to move between pages. You're encouraged to change the source files, the style sheet, the template, or @filepath{pollen.rkt}, and see how these changes immediately affect the page rendering in the project server. (You can also change the sequence of the pages in @filepath{index.ptree}, but in that case, you'll need to restart the project server to see the change.)

This page isn't a miracle of web design, but it shows you in one example:

@itemlist[

@item{Pollen markup being decoded — paragraph breaks, linebreaks, smart quotes, smart dashes — with a @racket[decode] function attached to the @code{root} node by @filepath{pollen.rkt};}

@item{A dynamically-generated CSS file that computes positions for CSS elements using numerical values set up with @racket[define], and mathematical conversions thereof;}

@item{Navigational links that appear and disappear as needed using conditional statements (@racket[when/block]) in @filepath{template.html}, with the page sequence defined by @filepath{index.ptree} and the names of the links being pulled from the @code{h1} tag of each source file using @racket[select].}

]

@section{Third tutorial complete}

OK, that was a humongous tutorial. Congratulations on making it through.

But your reward is that you now understand all the core concepts of the Pollen publishing system, including the most important ones: the flexibility of Pollen markup, and the connection between tags and functions.



