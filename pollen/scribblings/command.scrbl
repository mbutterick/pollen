#lang scribble/manual
@(require scribble/bnf scribble/eval "utils.rkt" "mb-tools.rkt"
          (for-syntax racket/base)
          (for-label rackunit pollen/core pollen/setup pollen/render pollen/template (only-in scribble/reader
                              use-at-readtable)))

@(define read-eval (make-base-eval))
@(interaction-eval #:eval read-eval (require (for-syntax racket/base)))

@(define (at-exp-racket)
   @racket[#, @hash-lang[] #, @racketmodname[at-exp] #, @racketidfont{racket}])

@title[#:tag "pollen-command-syntax"]{Pollen command syntax}

@section{The golden rule}

Pollen uses a special character — the @italic{lozenge}, which looks like this: ◊ — to mark commands  within a Pollen source file. So when you put a ◊ in your source, whatever comes next will be treated as a command. If you don't, it will just be interpreted as plain text.


@section{The lozenge glyph (◊)}

I chose the lozenge as the command character because a) it appears in almost every font, b) it's barely used in ordinary typesetting, c) it's not used in any programming language that I know of, and d) its shape and color allow it to stand out easily in code without being distracting. 

Here's how you type it:

@bold{Mac}: option + shift + V

@bold{Windows}: holding down alt, type 9674 on the num pad

@bold{Ubuntu}: ctrl + shift + U, then 25CA

Still, if you don't want to use the lozenge as your command character, you can set Pollen's @racket[setup:default-command-char] value to whatever character you want (see also @seclink["setup-overrides"]).

@margin-note{Scribble uses the @"@" sign as a delimiter. It's not a bad choice if you only work with Racket files. But as you use Pollen to work on other kinds of text-based files that commonly contain @"@" signs — HTML pages especially — it gets cumbersome. So I changed it.}

But don't knock the lozenge till you try it. 

@subsection{Lozenge helpers}

@subsubsection{DrRacket}

When you use DrRacket, you'll see a button in the toolbar that says @onscreen{Insert command char}. This will insert the lozenge (or whatever command character you've defined for your project).


@subsubsection{AHK script}

Courtesy of @link["https://github.com/maphew"]{Matt Wilkie}: ``Here's a working AHK script to have double-tap backtick send the lozenge character. It took way more time than I want to think about, once started I couldn't let go.''

@fileblock["tick-tick-lozenge.ahk"]{
@foreign-code{
;
; Double-tap backtick sends Pollen command character (◊)
; For some reason I needed to use Unicode 'U+25CA' 
; instead of the standard alt-code '9674'
;
; Adapted from http://ahkscript.org/docs/commands/SetTimer.htm
; and http://www.autohotkey.com/board/topic/145507-how-to-use-double-tap-backtick-with-pass-through/
;
#EscapeChar \
$`::
if winc_presses > 0 
    {winc_presses += 1
    return
    }

winc_presses = 1
SetTimer, TheKey, 250 ; Wait for more presses, in milliseconds
return

TheKey:
    SetTimer, TheKey, off
    if winc_presses = 1 ; The key was pressed once.
        {Send `
        }
    else if winc_presses = 2 ; The key was pressed twice.
        {Send {U+25CA}
        }

; Regardless of which action above was triggered, reset the count to
; prepare for the next series of presses:
winc_presses = 0
return}}


@subsubsection{Emacs script}

Courtesy of @link["https://github.com/lerichard95"]{Richard Le}: ``If you're using Emacs, I tried to write a tiny function that inserts the lozenge character. I chose M-\ because that's the key for the lambda character in DrRacket.''

@foreign-code{
;; Put this in your Emacs .init file:
;; enable lozenge for Pollen
;; ◊◊◊◊◊◊◊◊◊◊◊◊◊
;; 'mule-unicode part from
;; https://lists.gnu.org/archive/html//emacs-devel/2005-03/msg01187.html
(defun insert-lozenge ()
  "inserts the lozenge character for use with Pollen"
  ;; enables function through M-x
  (interactive)
  ;; insert the proper character
  (insert (make-char
           'mule-unicode-2500-33ff 34 42)))

;; Bind key to M-\ a la DrRacket for lambda
(global-set-key "\M-\\" 'insert-lozenge)}

@;--------------------------------------------------------------------
@section[#:tag "the-two-command-modes"]{The two command modes: Pollen mode & Racket mode}

Pollen commands can be entered in one of two modes: @italic{Pollen mode} or @italic{Racket mode}. Both modes start with a lozenge (@litchar["◊"]):

@racketblock[
 @#,BNF-seq[@litchar["◊"] @nonterm{command name} @litchar{[} @nonterm{Racket arguments ...} @litchar{]} @litchar["{"] @nonterm{text argument} @litchar["}"]]
@#,BNF-seq[@litchar["◊"]
            @litchar{(} @nonterm{Racket expression} @litchar{)}]
]

@bold{Pollen-mode commands}

A Pollen-mode command has the three possible parts after the @litchar["◊"]:

@itemlist[
@item{The @italic{command name} appears immediately after the @litchar["◊"]. Typically it's a short word.} 
@item{The @italic{Racket arguments} appear between square brackets. Pollen is partly an interface to the Racket programming language. These arguments are entered using Racket conventions — e.g., a @code{string of text} needs to be put in quotes as a @code{"string of text"}. If you like programming, you'll end up using these frequently. If you don't, you won't.}
@item{The @italic{text argument} appears between braces (aka curly brackets). You can put any ordinary text here. Unlike with the Racket arguments, you don't put quotes around the text.}
]

Each of the three parts is optional. You can also nest commands within each other. However:

@itemlist[
@item{You can never have spaces between the three parts.}
@item{Whatever parts you use must always appear in the order above.}
]

Here are a few examples of correct Pollen-mode commands:

@codeblock{
  #lang pollen
  ◊variable-name
  ◊tag{Text inside the tag.}
  ◊tag[#:attr "value"]{Text inside the tag}
  ◊get-customer-id["Brennan Huff"]
  ◊tag{His ID is ◊get-customer-id["Brennan Huff"].}
}

And some incorrect examples:

@codeblock{
  #lang pollen
  ◊tag {Text inside the tag.} ; space between first and second parts
  ◊tag[Text inside the tag] ; text argument needs to be within braces
  ◊tag{Text inside the tag}[#:attr "value"] ; wrong order 
}

The next section describes each of these parts in detail.

@bold{Racket-mode commands}

If you're familiar with Racket expressions, you can use the Racket-mode commands to embed them within Pollen source files. It's simple: any Racket expression can become a Pollen command by adding @litchar["◊"] to the front. So in Racket, this code:

@codeblock{
  #lang racket
  (define song "Revolution")
  (format "~a #~a" song (* 3 3))
}

Can be converted to Pollen like so: 

@codeblock{
  #lang pollen
  ◊(define song "Revolution")
  ◊(format "~a #~a" song (* 3 3))
}

And in DrRacket, they produce the same output:

@repl-output{Revolution #9}


Beyond that, there's not much to say about Racket mode — any valid expression you can write in Racket will also be a valid Racket-mode Pollen command.

@bold{The relationship of Pollen mode and Racket mode}

Even if you don't plan to write a lot of Racket-mode commands, you should be aware that under the hood, Pollen is converting all commands in Pollen mode to Racket mode. So a Pollen-mode command that looks like this:

@codeblock[#:keep-lang-line? #f]{
#lang pollen
◊headline[#:size 'enormous]{Man Bites Dog!}
}


Is actually being turned into a Racket-mode command like this:

@codeblock[#:keep-lang-line? #f]{
#lang racket
(headline #:size 'enormous "Man Bites Dog!")
}

Thus a Pollen-mode command is just an alternate way of writing a Racket-mode command. (More broadly, all of Pollen is just an alternate way of using Racket.)

The corollary is that you can always write Pollen commands using whichever mode is more convenient or readable. For instance, the earlier example, written in the Racket mode:

@codeblock{
#lang pollen
◊(define song "Revolution")
◊(format "~a #~a" song (* 3 3))
}

Can be rewritten using Pollen mode:

@codeblock{
#lang pollen
◊define[song]{Revolution}
◊format["~a #~a" song (* 3 3)]
}

And it will work the same way.


@;--------------------------------------------------------------------
@subsection{The command name}

In Pollen, you'll typically use the command name for one of four purposes:

@itemlist[
@item{To invoke a tag function.}
@item{To invoke another function.}
@item{To insert the value of a variable.}
@item{To insert a @code{meta} value.}
@item{To insert a comment.}
]

@;--------------------------------------------------------------------
@subsubsection{Invoking tag functions}

By default, Pollen treats every command name as a @italic{tag function}. The default tag function creates a tagged X-expression with the command name as the tag, and the text argument as the content.

@codeblock{
#lang pollen
◊strong{Fancy Sauce, $1} 
}

@repl-output{'(strong "Fancy Sauce, $1")}

To streamline markup, Pollen doesn't restrict you to a certain set of tags, nor does it make you define your tags ahead of time. Just type a tag, and you can start using it.


@codeblock{
  #lang pollen
  ◊utterlyridiculoustagname{Oh really?}
}

@repl-output{'(utterlyridiculoustagname "Oh really?")}



The one restriction is that you can't invent names for tags that are already being used for other commands. For instance, @code{map} is a name permanently reserved by the Racket function @racket[map]. It's also a rarely-used HTML tag. But gosh, you really want to use it. Problem is, if you invoke it directly, Pollen will think you mean the other @racket[map]: 


@codeblock{
#lang pollen
◊map{Fancy Sauce, $1} 
}

@errorblock{
map: arity mismatch;
the expected number of arguments does not match the given number
  given: 1
  arguments...:
    "Fancy Sauce, $1"}
   
What to do? Read on.

@;--------------------------------------------------------------------
@subsubsection{Invoking other functions}

Though every command name starts out as a default tag function, it doesn't necessarily end there. You have two options for invoking other functions: defining your own, or invoking others from Racket.

@bold{Defining your own functions}

Use the @racket[define] command to create your own function for a command name. After that, when you use the command name, you'll get the new behavior. For instance, recall this example showing the default tag-function behavior:

@codeblock{
#lang pollen
◊strong{Fancy Sauce, $1} 
}

@repl-output{'(strong "Fancy Sauce, $1")}

We can define @code{strong} to do something else, like add to the text:

@codeblock{
#lang pollen
◊(define (strong text) `(strong ,(format "Hey! Listen up! ~a" text)))
◊strong{Fancy Sauce, $1} 
}

@repl-output{'(strong "Hey! Listen up! Fancy Sauce, $1")}

The replacement function has to accept any arguments that might get passed along, but it doesn't have to do anything with them. For instance, this function definition won't work because @code{strong} is going to get a text argument that it's not defined to handle:

@codeblock{
#lang pollen
◊(define (strong) '(fib "1 1 2 3 5 8 13 ..."))
◊strong{Fancy Sauce, $1} 
}

@errorblock{strong: arity mismatch;
the expected number of arguments does not match the given number
  expected: 0
  given: 1
  arguments...:
    "Fancy Sauce, $1"}

Whereas in this version, @code{strong} accepts an argument called @code{text}, but then ignores it:

@codeblock|{
  #lang pollen
  ◊(define (strong text) '(fib "1 1 2 3 5 8 13 ..."))
  ◊strong{Fancy Sauce, $1} 
}|

@repl-output{'(fib "1 1 2 3 5 8 13 ...")}


You can attach any behavior to a command name. As your project evolves, you can also update the behavior of a command name. In that way, Pollen commands become a set of hooks to which you can attach more elaborate processing.

@bold{Using Racket functions}

You aren't limited to functions you define. Any function from Racket, or any Racket library, can be invoked directly by using it as a command name. Here's the function @racket[range], which creates a list of numbers:

@codeblock|{
#lang pollen
◊range[1 20]
}|

@repl-output{'(range 1 20)}

Hold on — that's not what we want. Where's the list of numbers? The problem here is that we didn't explicitly import the @racketmodname[racket/list] library, which contains the definition for @racket[range]. (If you need to find out what library contains a certain function, the Racket documentation will tell you.) Without @racketmodname[racket/list], Pollen just thinks we're trying to use @code{range} as a tag function (and if we had been, then @repl-output{'(range 1 20)} would've been the right result). 

We fix this by using the @racket[require] command to bring in the @racketmodname[racket/list] library, which contains the @racket[range]  we want:

@codeblock|{
#lang pollen
◊(require racket/list)
◊range[1 20]
}|

@repl-output{'(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)}

Of course, you can also invoke Racket functions indirectly, by attaching them to functions you define for command names:

@codeblock|{
#lang pollen
◊(require racket/list)
◊(define (rick start finish) (range start finish))
◊rick[1 20]
}|

@repl-output{'(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)}


Let's return to the problem that surfaced in the last section — the fact that some command names can't be used as tag functions because they're already being used for other things. You can work around this by defining your own tag function with a non-conflicting name. 

For instance, suppose we want to use @code{map} as a tag even though Racket is using it for its own function called @racket[map]. First, we invent a command name that doesn't conflict. Let's call it @code{my-map}. As you learned above, Pollen will treat a new command name as a tag function by default:

@codeblock|{
#lang pollen
◊my-map{How I would love this to be a map.}
}|

@repl-output{'(my-map "How I would love this to be a map.")}


But @code{my-map} is not the tag we want. We need to define @code{my-map} to be a tag function for @code{map}. We can do this with the Pollen helper @racket[default-tag-function]. That function lives in @racket[pollen/tag], so we @racket[require] that too:


@codeblock|{
#lang pollen
◊(require pollen/tag)
◊(define my-map (default-tag-function 'map))
◊my-map{How I would love this to be a map.}
}|

@repl-output{'(map "How I would love this to be a map.")}

Problem solved.



@;--------------------------------------------------------------------
@subsubsection{Inserting the value of a variable}

A Pollen command name usually refers to a function, but it can also refer to a @italic{variable}, which is a data value. Once you define the variable, you can insert it into your source by using the ◊ notation without any other arguments:

@codeblock|{
#lang pollen
◊(define foo "bar")
The value of foo is ◊foo
}|

@repl-output{The value of foo is bar}


Be careful — if you include arguments, even blank ones, Pollen will treat the command name as a function. This won't work, because a variable is not a function:

@margin-note{To understand what happens here, recall the relationship between Pollen's command modes. The Pollen-mode command @code{◊foo[]} becomes the Racket-mode command @code{(foo)}, which after variable substitution becomes @code{("bar")}. If you try to evaluate @code{("bar")} — e.g., in DrRacket — you'll get the same error.}


@codeblock|{
#lang pollen
◊(define foo "bar")
The value of foo is ◊foo[]
}|


@errorblock{application: not a procedure;
expected a procedure that can be applied to arguments
  given: "bar"
  arguments...: [none]}


The reason we can simply drop @code{◊foo} into the text argument of another Pollen command is that the variable @code{foo} holds a string (i.e., a text value). 

In preprocessor source files, Pollen will convert a variable to a string in a sensible way. For instance, numbers are easily converted:

@codeblock|{
#lang pollen
◊(define zam 42)
The value of zam is ◊zam
}|

@repl-output{The value of zam is 42}

@margin-note{In an unsaved DrRacket file, or a file without a special Pollen source extension, the @tt{#lang pollen} designation invokes the Pollen preprocessor by default. You can explicitly invoke preprocessor mode by starting a file with @tt{#lang pollen/pre}. See also @secref["Preprocessor___pp_extension_"].}

If the variable holds a container datatype (like a @racket[list], @racket[hash], or @racket[vector]), Pollen will produce the Racket text representation of the item. Here, @code{zam} is a @racket[list] of integers:

@codeblock|{
#lang pollen
◊(define zam (list 1 2 3))
The value of zam is ◊zam
}|

@repl-output{The value of zam is '(1 2 3)}

This feature is included for your convenience. But in general, your readers won't want to see the Racket representation of a container. So in these cases, you should convert to a string manually in some sensible way. Here, the integers in the list are converted to strings, which are then combined using @racket[string-join] from the @racketmodname[racket/string] library:

@codeblock|{
#lang pollen
◊(require racket/string)
◊(define zam (list 1 2 3))
The value of zam is ◊string-join[(map number->string zam)]{ and }
}|

@repl-output{The value of zam is 1 and 2 and 3}

Pollen will still produce an error if you try to convert an esoteric value to a string. Here, @code{zam} is the addition function (@racket[+]):

@codeblock|{
#lang pollen
◊(define zam +)
The value of zam is ◊zam
}|

@errorblock{Pollen decoder: can't convert #<procedure:+> to string}

Moreover, Pollen will not perform @italic{any} automatic text conversion in Pollen markup source files. Suppose we take the example above — which worked as a preprocessor source file — and change the language to @racket[pollen/markup]:

@codeblock|{
#lang pollen/markup
◊(define zam (list 1 2 3))
The value of zam is ◊zam
}|

This time, the file will produce an error:

@errorblock{
  pollen markup error: in '(root "The value of zam is " (1 2 3)), '(1 2 3) is not a valid element (must be txexpr, string, symbol, XML char, or cdata)
}

One special case to know about. In the examples above, there's a word space between the variable and the other text. But suppose you need to insert a variable into text so that there's no space in between. The simple ◊ notation won't work, because it won't be clear where the variable name ends and the text begins. 

For instance, suppose we want to use a  variable @code{edge} next to the string @code{px}:

@codeblock|{
#lang pollen
◊(define edge 100)
p { margin-left: ◊edgepx; }
}|

@errorblock{Pollen decoder: can't convert #<procedure:...t/pollen/tag.rkt:6:2> to string}

The example fails because Pollen reads the whole string after the @litchar{◊} as the single variable name @code{edgepx}. Since @code{edgepx} isn't defined, it's treated as a tag function, and since Pollen can't convert a function to a string, we get an error.

In these situations, surround the variable name with vertical bars @litchar{◊|}like so@litchar{|} to explicitly indicate where the variable name ends. The bars are not treated as part of the name, nor are they included in the result. Once we do that, we get what we intended:

@codeblock|{
#lang pollen
◊(define edge 100)
p { margin-left: ◊|edge|px; }
}|

@repl-output{p { margin-left: 100px; }}

If you use this notation when you don't need to, nothing bad will happen. The vertical bars are always ignored.

@codeblock|{
#lang pollen
◊(define edge 100)
The value of edge is ◊|edge| pixels}
}|

@repl-output{The value of edge is 100 pixels}




@;--------------------------------------------------------------------
@subsubsection{Inserting metas}

@italic{Metas} are key–value pairs embedded in a source file that are not included in the main output when the source is compiled. Rather, they're gathered and exported as a separate hash table called, unsurprisingly, @racket[metas]. This hashtable is a good place to store information about the document that you might want to use later (for instance, a list of topic categories that the document belongs to).

@margin-note{Pollen occasionally uses metas internally. For instance, the @racket[get-template-for] function will look in the metas of a source file to see if a template is explicitly specified. The @racket[pollen/template] module also contains functions for working with metas, such as @racket[select-from-metas].}

To make a meta, you create a tag with the special @racket[define-meta] name. Then you have two choices: you can either embed the key-value pair as an attribute, or as a tagged X-expression within the meta (using the key as the tag, and the value as the body):

@codeblock{
#lang pollen

◊define-meta[dog]{Roxy} ; Pollen-mode syntax
◊some-tag[#:key "value"]{Normal tag}
◊(define-meta cat "Chopper") ; equivalent Racket-mode syntax
◊some-tag[#:key "value"]{Another normal tag}
}

When you run a source file with metas in it, two things happen. First, the metas are removed from the output:

@repl-output{
'(some-tag ((key "value")) "Normal tag")

'(some-tag ((key "value")) "Another normal tag")
}


Second, the metas are collected into a hash table that is exported with the name @code{metas}. To see this hash table, run the file above in DrRacket, then switch to the interactions window and type @exec{metas} at the prompt:

@terminal{
> metas
'#hasheq((dog . "Roxy") (cat . "Chopper") (here-path . "unsaved-editor"))
}

The only key that's automatically defined in every meta table is @code{here-path}, which is the absolute path to the source file. (In this case, because the file hasn't been saved, you'll see the @code{unsaved-editor} name instead.) 

Still, you can override this too:

@codeblock{
#lang pollen

◊define-meta[dog]{Roxy}
◊some-tag[#:key "value"]{Normal tag}
◊(define-meta cat "Chopper")
◊some-tag[#:key "value"]{Another normal tag}
◊(define-meta here-path "tesseract")
}

When you run this code, the result will be the same as before, but this time the metas will be different:

@terminal{
> metas
'#hasheq((dog . "Roxy") (cat . "Chopper") (here-path . "tesseract"))
}


It doesn't matter how many metas you put in a source file, nor where you put them. They'll all be extracted into the @code{metas} hash table. The order of the metas is not preserved (because order is not preserved in a hash table). But if you have two metas with the same key, the later one will supersede the earlier one:

@codeblock{
#lang pollen
◊define-meta[dog]{Roxy}
◊(define-meta dog "Lex")
}

In this case, though there are two metas named @racket[dog] (and they use different forms) only the second one persists:

@terminal{
> metas
'#hasheq((dog . "Lex") (here-path . "unsaved-editor"))
}

@bold{Pro tip}: the @racket[metas] hashtable is available when you import a Pollen source file in the usual way, but it's also made available through a submodule called, unsurprisingly, @racket[metas].

@codeblock{
#lang racket/base
(require "pollen-source.rkt") ; doc and metas and everything else
(require (submod "pollen-source.rkt" metas)) ; just metas
}

The @racket[metas] submodule is useful because it gives you access to the @racket[metas] hashtable @italic{without} compiling the rest of the file. So if you need to collect metas from a set of source files — for instance, page titles (for a table of contents) or categories — getting the metas through the submodule is likely to be faster.


@;--------------------------------------------------------------------
@subsubsection{Retrieving metas}

The @racket[metas] hashtable is available immediately within the body of your source file. You can use @racket[hash-ref] to get values out of @racket[metas].

@codeblock{
#lang pollen
◊(define-meta dog "Roxy")
◊(hash-ref metas 'dog)
}

@terminal{
Roxy
}


Because the metas are collected first, you can actually invoke a meta before you define it:

@codeblock{
#lang pollen
◊(hash-ref metas 'dog)
◊(define-meta dog "Roxy")
◊(define-meta dog "Spooky")
}

@terminal{
Spooky
}

This can be useful for setting up fields that you want to include in @racket[metas] but also have visible in the body of a document, like a title.

@codeblock{
#lang pollen/markup
◊(define-meta title "The Amazing Truth")
◊h1{◊(hash-ref metas 'title)}
}

The result of this file will be:

@terminal{
'(root (h1 "The Amazing Truth"))
}

And the metas:
@terminal{
> metas
'#hasheq((title . "The Amazing Truth") (here-path . "unsaved-editor"))
}
You cannot, however, use @racket[hash-set!] or other similar functions, because @racket[metas] is an immutable hash.



@;--------------------------------------------------------------------
@subsubsection{Inserting a comment}

Two options.

To comment out the rest of a single line, use a lozenge followed by a semicolon @litchar{◊;}.

@codeblock|{
#lang pollen
◊span{This is not a comment}
◊span{Nor is this} ◊;span{But this is}
}|

@repl-output{'(span "This is not a comment") 
'(span "Nor is this")}

To comment out a multiline block, use the lozenge–semicolon signal @litchar{◊;} with curly braces, @litchar{◊;@"{"}like so@litchar{@"}"}.

@codeblock|{
#lang pollen
◊;{
◊span{This is not a comment}
◊span{Nor is this} ◊;span{But this is}
}
Actually, it's all a comment now
}|


@repl-output{Actually, it's all a comment now}

@;--------------------------------------------------------------------
@subsection{The Racket arguments}

The middle part of a Pollen-mode command contains the @italic{Racket arguments} @litchar{[}between square brackets.@litchar{]} Most often, you'll see these used to pass extra information to commands that operate on text.

For instance, tag functions. Recall from before that any not-yet-defined command name in Pollen is treated as a tag function:

@codeblock|{
#lang pollen
◊title{The Beginning of the End}
}|

@repl-output{'(title "The Beginning of the End")}

But what if you wanted to add attributes to this tag, so that it comes out like this?

@repl-output{'(title ((class "red")(id "first")) "The Beginning of the End")}

You can do it with Racket arguments. 

Here's the hard way. You can type out your list of attributes in Racket format and drop them into the brackets as a single argument:

@codeblock|{
#lang pollen
◊title['((class "red")(id "first"))]{The Beginning of the End}
}|

@repl-output{'(title ((class "red") (id "first")) "The Beginning of the End")}


But that's a lot of parentheses to think about. So here's the easy way. Anytime you use a tag function, there's a shortcut for inserting attributes. You can enter them as a series of @italic{keyword arguments} between the Racket-argument brackets. The only caveat is that the values for these keyword arguments have to be strings. So taken together, they look like this:

@codeblock|{
#lang pollen
◊title[#:class "red" #:id "first"]{The Beginning of the End}
}|

@repl-output{'(title ((class "red") (id "first")) "The Beginning of the End")}

The string arguments can be any valid Racket expressions that produce strings. For instance, this will also work:

@codeblock|{
#lang pollen
◊title[#:class (format "~a" (* 6 7)) #:id "first"]{The Beginning of the End}
}|

@repl-output{'(title ((class "42") (id "first")) "The Beginning of the End")}

Since Pollen commands are really just Racket arguments underneath, you can use those too. Here, we'll define a variable called @code{name} and use it in the Racket arguments of @code{title}:

@codeblock|{
#lang pollen
◊(define name "Brennan")
◊title[#:class "red" #:id ◊name]{The Beginning of the End}
}|

@repl-output{'(title ((class "read") (id "Brennan")) "The Beginning of the End")}

When used in custom tag functions, keyword arguments don't have to represent attributes. Instead, they can be used to provide options for a particular Pollen command, to avoid redundancy. Suppose that instead of using the @code{h1 ... h6} tags, you want to consolidate them into one command called @code{heading} and select the level separately. You can do this with a keyword, in this case @racket[#:level], which is passed as a Racket argument:

@codeblock|{
#lang pollen
◊(define (heading #:level which text)
   `(,(string->symbol (format "h~a" which)) ,text))

◊heading[#:level 1]{Major league}
◊heading[#:level 2]{Minor league}
◊heading[#:level 6]{Trivial league}
}|

@repl-output{'(h1 "Major league")
'(h2 "Minor league")
'(h6 "Trivial league")
}

@;--------------------------------------------------------------------
@subsection{The text argument}

The third part of a Pollen-mode command is the text argument. The text argument @litchar{@"{"}appears between curly braces@litchar{@"}"}. It can contain any text you want. The text argument can also contain other Pollen commands with their own text arguments. And they can contain other Pollen commands ... and so on, all the way down.

@codeblock|{
#lang pollen
◊div{Do it again. ◊div{And again. ◊div{And yet again.}}}
}|

@repl-output{'(div "Do it again. " (div "And again. " (div "And yet again.")))}

Three small details to know about the text argument.

First, the only character that needs special handling in a text argument is the lozenge @litchar{◊}. A lozenge ordinarily marks a new command. So if you want an actual lozenge to appear in the text, you have to escape it by typing @litchar{◊"◊"}.

@codeblock|{
#lang pollen
◊definition{This is the lozenge: ◊"◊"}
}|

@repl-output{'(definition "This is the lozenge: ◊")}

Second, the whitespace-trimming policy. Here's the short version: if there's a carriage return at either end of the text argument, it is trimmed, and whitespace at the end of each line is selectively trimmed in an intelligent way. So this text argument, with carriage returns on the ends:

@codeblock|{
#lang pollen
◊div{
Roomy!
     
I agree.
}
}|

@repl-output{'(div "Roomy!" "\n" "\n" "I agree.")}

Yields the same result as this one:

@codeblock|{
#lang pollen
◊div{Roomy!
     
I agree.}
}|

@repl-output{'(div "Roomy!" "\n" "\n" "I agree.")}

For the long version, please see [future link: Spaces, Newlines, and Indentation].


Third, within a multiline text argument, newline characters become individual strings that are not merged with adjacent text. So what you end up with is a list of strings, not a single string. That's why in the last example, we got this:

@repl-output{'(div "Roomy!" "\n" "\n" "I agree.")}

Instead of this:

@repl-output{'(div "Roomy!\n\nI agree.")}

Under most circumstances, these two tagged X-expressions will behave the same way. The biggest exception is with functions. A function that operates on multiline text arguments needs to be able to handle an indefinite number of strings. For instance, this @code{jejune} function only accepts a single argument. It will work with a single-line text argument, because that produces a single string:

@codeblock|{
#lang pollen
◊(define (jejune text)
   `(jejune ,text))
◊jejune{Irrational confidence}
}|

@repl-output{'(jejune "Irrational confidence")}

But watch what happens with a multiline text argument:

@codeblock|{
#lang pollen
◊(define (jejune text)
   `(jejune ,text))
◊jejune{Deeply
        chastened}
}|

@errorblock{jejune: arity mismatch;
the expected number of arguments does not match the given number
  expected: 1
  given: 3
  arguments...:
   "Deeply"
   "\n"
   "chastened"}
 
The answer is to use a @italic{rest argument} in the function, which takes the ``rest'' of the arguments — however many there may be — and combines them into a single @racket[list]. If we rewrite  @code{jejune} with a rest argument, we can fix the problem:

@codeblock|{
#lang pollen
◊(define (jejune . texts)
   `(jejune ,@texts))
◊jejune{Deeply
        chastened}
}|

@repl-output{'(jejune "Deeply" "\n" "chastened")}


@section{Embedding character entities}

XML and HTML support @italic{character entities}, a way of encoding Unicode characters with a name or number. For instance, in HTML, the copyright symbol @litchar{©} can be encoded by name as @tt{&copy;} or by number as @tt{&#169;}.

Entities originated as a way of embedding Unicode characters in an ASCII data stream. Pollen and Racket, however, support Unicode directly. So does every major web browser (though your document may need a Unicode character-set declaration to trigger it). So usually, your best bet is insert Unicode characters directly into your source rather than use entities.

But if you really need entities, here's what to do. Pollen treats everything as text by default, so you can't insert entities merely by typing them, because they'll just be converted to text:

@codeblock|{
#lang pollen
◊div{copy
     169}
}|

@repl-output{'(div "copy" "\n" "169")}

Instead, named entities are handled as @secref["symbols" #:doc '(lib "scribblings/guide/guide.scrbl")] and numeric entities are, unsurprisingly, @secref["numbers" #:doc '(lib "scribblings/guide/guide.scrbl")]. So you can use the @racket[string->symbol] and @racket[string->number] functions to convert your entity input:

@codeblock|{
#lang pollen
◊div{◊string->symbol{copy}
     ◊string->number{169}}
}|

@repl-output{'(div copy "\n" 169)}

Notice that in the output, there are no more quote marks around @tt{copy} and @tt{169}, indicating that they're not strings. When you pass this result to a converter like @racket[->html], the entities will be escaped correctly:

@codeblock|{
#lang pollen
◊(require pollen/template)

◊->html{◊div{copy 169}}

◊->html{◊div{◊string->symbol{copy} ◊string->number{169}}}
}|

@repl-output{<div>copy 169</div>

<div>&copy; &#169;</div>}

For numeric entities, you can also use a four-digit Unicode hex number by prefacing it with @litchar{#x}, which is the standard Racket prefix for a hex number:

@codeblock|{
#lang pollen
◊div{◊string->number{169}
     ◊string->number{#x00a9}}
}|

@repl-output{'(div 169 "\n" 169)}

Of course, you don't need to use @racket[string->symbol] and @racket[string->number] directly in your source. You can also define tag functions that generate entities. The key point is that to be treated as an entity, the return value must be a symbol or number, rather than a string.


@section{Adding Pollen-mode commands to a Racket file}

@defmodulelang[pollen/mode]

Just as you can embed any Racket-mode command in a Pollen source file, you can go the other way and embed Pollen-mode commands in a Racket file. For instance, in your @secref["The__pollen_rkt__file"], you may find it convenient to use Pollen mode for certain values.

You enable Pollen mode within your source file by adding @racketmodname[pollen/mode] to your @tt{#lang} line at the top of your source:

@fileblock["pollen.rkt" @codeblock{
#lang pollen/mode racket/base
(require pollen/tag)

(define link (default-tag-function 'a))

(define (home-link)
  (link #:href "index.html" "Click to go home"))

(define (home-link-pollen-mode)
  ◊link[#:href "index.html"]{Click to go home})

}]

Here, both @tt{(home-link)} and @tt{(home-link-pollen-mode)} will produce the same X-expression as a result:

@terminal{'(a ((href "index.html")) "Click to go home")}

Of course, you can use @racketmodname[pollen/mode] in any Racket source file, not just @filepath{pollen.rkt}. 

@bold{Major caveat}: @racketmodname[pollen/mode] only works with Pollen's default command character, namely the lozenge (@litchar{◊}). If you've overridden this command character in your @filepath{pollen.rkt} file, your custom command character will work everywhere @italic{except} in @racketmodname[pollen/mode]. This limitation is necessary to prevent the intractable situation where @filepath{pollen.rkt} relies on @racketmodname[pollen/mode], but @racketmodname[pollen/mode] relies on a config setting in @filepath{pollen.rkt}.

Also keep in mind that @racketmodname[pollen/mode] is just a syntactic convenience. It doesn't change any of the underlying semantics of your Racket source file. Your Pollen-mode commands are being translated into Racket commands and compiled along with everything else.

Another good way to use Pollen-mode commands in Racket is for unit tests with @racketmodname[rackunit]. With @racketmodname[pollen/mode], you can write your unit tests in Pollen mode or Racket mode (or mix them).

@margin-note{Unit tests are little one-line tests you put into your code to verify that it does what you expect. You make these with the @racketmodname[rackunit] library, which is beloved by all Racket programmers. For more, see @secref["quick-start" #:doc '(lib "rackunit/scribblings/rackunit.scrbl")].}

@fileblock["pollen.rkt" @codeblock|{
#lang pollen/mode racket/base
(require rackunit)

(define (tag-fn arg . text-args)
  `(div ((class ,arg)) ,@text-args))

(check-equal? ◊tag-fn["42"]{hello world}
              '(div ((class "42")) "hello world"))

(check-equal? (tag-fn "42" "hello world")
              '(div ((class "42")) "hello world"))

(check-equal? ◊tag-fn["42"]{hello world}
              ◊'div[((class "42"))]{hello world})

}|]


@section{Further reading}

The Pollen language is a variant of Racket's own text-processing language, called Scribble. Thus, most things that can be done with Scribble syntax can also be done with Pollen syntax. For the sake of clarity & brevity, I've only shown you the highlights here. But if you want the full story, see @secref["reader" #:doc '(lib "scribblings/scribble/scribble.scrbl")] in the Scribble documentation.