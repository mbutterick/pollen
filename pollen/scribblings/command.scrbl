#lang scribble/manual
@(require scribble/bnf scribble/eval "utils.rkt" "mb-tools.rkt" pollen/setup
          (for-syntax racket/base)
          (for-label rackunit pollen/core pollen/setup pollen/cache pollen/tag pollen/render pollen/template (only-in scribble/reader
                              use-at-readtable)))

@(define read-eval (make-base-eval))
@(interaction-eval #:eval read-eval (require (for-syntax racket/base)))

@(define (at-exp-racket)
   @racket[#, @hash-lang[] #, @racketmodname[at-exp] #, @racketidfont{racket}])

@title[#:tag "pollen-command-syntax"]{Pollen command syntax}

@section{The golden rule}

Pollen uses a special character — the @italic{lozenge}, which looks like this: ◊ — to mark commands  within a Pollen source file. So when you put a ◊ in your source, whatever comes next will be treated as a command. If you don't, it will just be interpreted as plain text.


@section[#:tag "the-lozenge"]{The lozenge (◊)}

I chose the lozenge as the command character because a) it appears in almost every font, b) it's barely used in ordinary typesetting, c) it's not used in any programming language that I know of, and d) its shape and color allow it to stand out easily in code without being distracting. 

Consideration (b) is especially important in a text-based language like Pollen. If Pollen used something more common as its command character, then every time you used that character in text, you'd have to specially escape it. This would make it cumbersome and annoying to import plain text into Pollen source files. This is the Pareto-optimal trade.

If you're using DrRacket, you can use the @onscreen{Insert Command Char} button at the top of the editing window to — you guessed it — insert the command character.

If you're using a different editor, here's how you type it:

@bold{Mac}: Option + Shift + V
@(linebreak)@bold{Windows}: holding down Alt, type 9674 on the num pad
@(linebreak)@bold{GNU/Linux, BSD}: Type Ctrl + Shift + U, then 25CA, then Enter

For more information on entering arbitrary Unicode characters, see @link["https://en.wikipedia.org/wiki/Unicode_input"]{Wikipedia}.

@subsection{``But I don't want to use the lozenge ...''}

Fine, but you have to pick @italic{something} as your command character. If you don't like this one, you can override it within a project — see @seclink["setup-overrides"].

Still, it's not like I'm asking you to learn @link["http://c2.com/cgi/wiki?AplLanguage"]{APL}. Racket supports Unicode, so it's a little silly to artificially limit ourselves to ASCII.

My advice: don't knock the lozenge till you try it.

@subsection{Lozenge helpers}

@subsubsection{How MB types the lozenge}

I work on Mac OS. I use @link["http://www.ergonis.com/products/typinator/"]{Typinator} to assign the @literal{◊} character to the otherwise never-used Option + Shift + backslash key combo (ordinarily assigned to @literal{»}). For that matter, I assign @literal{λ} to Option + backslash (ordinarily assigned to @literal{«}).

@subsubsection{DrRacket toolbar button}

When you use DrRacket, you'll see a button in the toolbar that says @onscreen{Insert command char}. This will insert the lozenge (or whatever command character you've defined for your project).

@subsubsection{DrRacket key shortcut}


Courtesy of @link["https://github.com/interstar"]{Phil Jones}: ``I created a file called @filepath{keys.rkt} —

@foreign-code{
#lang s-exp framework/keybinding-lang

(keybinding "c:<" (λ (editor evt) (send editor insert "◊")))
}


— and loaded it into DrRacket with @menuitem["Edit" "Keybindings | Add user-defined keybindings ..."]. Now I can use ctrl + shift + < to put the lozenge in.''

See also @secref["defining-shortcuts" #:doc '(lib "scribblings/drracket/drracket.scrbl")] in the DrRacket documentation.

@subsubsection{AHK script for Windows}

Courtesy of @link["https://github.com/maphew"]{Matt Wilkie}: ``Here's a working AHK script to have double-tap backtick send the lozenge character. It took way more time than I want to think about, once started I couldn't let go.''

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
return}

An alternative, courtesy of @link["http://www.barzilay.org"]{Eli Barzilay}: ``this turns M-\ to the lozenge'':

@foreign-code{!\:: Send {U+25CA}}


@subsubsection{Emacs script}

Courtesy of @link["https://github.com/lerichard95"]{Richard Le}: ``I chose M-\ because that's the key for the lambda character in DrRacket.'' (@link["http://www.barzilay.org"]{Eli Barzilay} shortened it.)

@foreign-code{(global-set-key "\M-\\" "◊")}

@subsubsection{Emacs input method}

Courtesy of @link["https://github.com/LemmingAvalanche"]{Kristoffer Haugsbakk}: ``Press @exec{C-\ rfc1345 RET} to choose the @racket[rfc1345] input method and toggle
it on. When this input method is toggled, @litchar{◊} can be produced by
entering @exec{&LZ}. The input method can be toggled on and off with @exec{C-\}.''

@subsubsection{Vim (and Evil) digraph sequence}

Courtesy of @link["https://github.com/LemmingAvalanche"]{Kristoffer Haugsbakk}: ``While in insert mode in Vim, or insert state in Evil (Emacs), press
@exec{C-k LZ} to enter @litchar{◊}. @exec{C-k} lets you enter a digraph (in
Vim terminology) which maps to another character. In this case, the
digraph @exec{LZ} maps to @litchar{◊}.

To make another mapping for this character in Vim, execute the following
command: @exec{:digraphs ll 9674} to (in this case) use the digraph @exec{ll}. @exec{9674} is the decimal representation of @litchar{◊} in Unicode.''

@subsubsection{Compose key}

Courtesy of @link["https://github.com/rrthomas"]{Reuben Thomas}: ``When using X11 (common on GNU/Linux and BSD systems), one can use the @link["https://en.wikipedia.org/wiki/Compose_key"]{Compose key}. It is often disabled by default; check your desktop environment's keyboard settings, or at a lower level, to use the Menu key as the Compose key:

@foreign-code{
setxkbmap -option compose:menu
}

See @exec{man xkeyboard-config} for all the ready-made options for the
compose key.

Since the lozenge character does not exist in the default compose-mapping file, you need to add this to your @filepath{~/.XCompose}:

@foreign-code{
   <Multi_key> <l> <l> : "◊"
}

See @exec{man XCompose}, or for more details, including many additional Compose bindings, see @link["https://github.com/rrthomas/pointless-xcompose"]{pointless-xcompose}.''

@section[#:tag "the-two-command-styles"]{The two command styles: Pollen style & Racket style}

Pollen commands can be entered in one of two styles: @italic{Pollen style} or @italic{Racket style}. Both styles start with a lozenge (@litchar["◊"]):

@racketblock[
 @#,BNF-seq[@litchar["◊"] @nonterm{command name} @litchar{[} @nonterm{Racket arguments ...} @litchar{]} @litchar["{"] @nonterm{text body ...} @litchar["}"]]
@#,BNF-seq[@litchar["◊"]
            @litchar{(} @nonterm{Racket expression} @litchar{)}]
]

@bold{Pollen-style commands}

A Pollen-style command has the three possible parts after the @litchar["◊"]:

@itemlist[
@item{The @italic{command name} appears immediately after the @litchar["◊"]. Typically it's a short word.} 

@item{The @italic{Racket arguments} appear between square brackets. Pollen is partly an interface to the Racket programming language. These arguments are entered using Racket conventions — e.g., a string of text needs to be put in quotes as a @code{"string of text"}. If you like programming, you'll end up using these arguments frequently. If you don't, you won't.}

@item{The @italic{text body} appears between braces (aka curly brackets). You can put any ordinary text here. Unlike with the Racket arguments, you don't put quotes around the text.}
]

Each of the three parts is optional. You can also nest commands within each other. However:

@itemlist[
@item{You can never have spaces between the three parts.}
@item{Whatever parts you use must always appear in the order above.}
]

Here are a few examples of correct Pollen-style commands:

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
  ◊tag[Text inside the tag] ; text body needs to be within braces
  ◊tag{Text inside the tag}[#:attr "value"] ; wrong order 
}

The next section describes each of these parts in detail.

@bold{Racket-style commands}

If you're familiar with Racket expressions, you can use the Racket-style commands to embed them within Pollen source files. It's simple: any Racket expression becomes a Pollen command by adding @litchar["◊"] to the front. So in Racket, this code:

@codeblock{
#lang racket
(define band "Level")
(format "~a ~a" band (* 2 3 7))
}

Can be converted to Pollen like so: 

@codeblock{
#lang pollen
◊(define band "Level")
◊(format "~a ~a" band (* 2 3 7))
}

And in DrRacket, they produce the same output:

@repl-output{Level 42}


Beyond that, there's not much to say about Racket style — any valid Racket expression will also be a valid Racket-style Pollen command.

@bold{The relationship of Pollen style and Racket style}

Even if you don't plan to write a lot of Racket-style commands, you should be aware that under the hood, Pollen is converting all Pollen-style commands to Racket style. So a Pollen-style command that looks like this:

@codeblock[#:keep-lang-line? #f]{
#lang pollen
◊headline[#:size 'enormous]{Man Bites Dog!}
}


Is actually being turned into this Racket-style command:

@codeblock[#:keep-lang-line? #f]{
#lang racket
(headline #:size 'enormous "Man Bites Dog!")
}

Thus a Pollen-style command is just an alternate way of writing a Racket-style command. (More broadly, all of Pollen is just an alternate way of using Racket.)

The corollary is that you can always write Pollen commands using whichever style is more convenient or readable. For instance, the earlier example, written in the Racket style:

@codeblock{
#lang pollen
◊(define band "Level")
◊(format "~a ~a" band (* 2 3 7))
}

Can be rewritten in Pollen style:

@codeblock{
#lang pollen
◊define[band]{Level}
◊format["~a ~a" band (* 2 3 7)]
}

And it will work the same way.

You can combine the two styles in whatever way makes sense to you. I typically reserve Pollen-style commands for when I'm mixing commands into textual material. Meaning, I prefer @code{◊headline[#:size 'enormous]{Man Bites Dog!}} over @code{◊(headline #:size 'enormous "Man Bites Dog!")}. But when I'm writing or using traditional Racket functions, I find Racket-style commands to be more readable (because they correspond to ordinary Racket syntax, and thus can be moved between Pollen and Racket source files more easily). So I prefer @code{◊(define band "Level")} over @code{◊define[band]{Level}}.


@subsection{The command name}

In Pollen, you'll likely use a command for one of these purposes:

@itemlist[
@item{To invoke a tag function.}
@item{To invoke another function.}
@item{To insert the value of a variable.}
@item{To insert a @code{meta} value.}
@item{To insert a comment.}
]

Let's look at each kind of use.

@subsubsection{Invoking tag functions}

By default, Pollen treats every command name as a @italic{tag function}. The default tag function creates a  @seclink["what-is-a-txexpr" #:doc '(lib "txexpr/scribblings/txexpr.scrbl")]{tagged X-expression} with the command name as the tag, and the text body as the content.

@codeblock{
#lang pollen
◊strong{Fancy Sauce, $1} 
}

@repl-output{'(strong "Fancy Sauce, $1")}

To streamline markup, Pollen doesn't restrict you to a certain set of tags, nor does it make you define your tags ahead of time. Just type a tag, and you can start using it.


@codeblock{
  #lang pollen
  ◊utterly-ridiculous-tag-name{Oh really?}
}

@repl-output{'(utterly-ridiculous-tag-name "Oh really?")}



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

The replacement function has to accept any arguments that might get passed along, but it doesn't have to do anything with them. For instance, this function definition won't work because @code{strong} is going to get a text body that it's not defined to handle:

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


@margin-note{The text body can pass an indefinite number of arguments. A well-designed tag function should be able to handle them, unlike these synthetic examples. For a more realistic example, see @secref["the-text-body"].}


You can attach any behavior to a command name. As your project evolves, you can also update the behavior of a command name. In that way, Pollen commands become a set of hooks to which you can attach more elaborate processing.

@bold{Using Racket functions}

You aren't limited to functions you define. Any function from Racket, or any Racket library, can be invoked directly by using it as a command name. Here's the function @racket[range], which creates a list of numbers:

@codeblock|{
#lang pollen
◊(range 1 20)
}|

@repl-output{'(range 1 20)}

Hold on — that's not what we want. Where's the list of numbers? The problem here is that we forgot to import the @racketmodname[racket/list] library, which contains the definition for @racket[range]. (If you need to find out what library contains a certain function, the Racket documentation will tell you.) Without @racketmodname[racket/list], Pollen just thinks we're trying to use @code{range} as a tag function (and if we had been, then @val['(range 1 20)] would've been the right result). 

We fix this by using the @racket[require] command to bring in the @racketmodname[racket/list] library, which contains the @racket[range]  we want:

@codeblock|{
#lang pollen
◊(require racket/list)
◊(range 1 20)
}|

@repl-output{'(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)}

Of course, you can also invoke Racket functions indirectly, by attaching them to functions you define for command names:

@codeblock|{
#lang pollen
◊(require racket/list)
◊(define (rick start finish) (range start finish))
◊(rick 1 20)
}|

@repl-output{'(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)}


Let's return to the problem that surfaced in the last section — the fact that some command names can't be used as tag functions because they're already being used for other things. You can work around this by defining your own tag function with a non-conflicting name. 

For instance, suppose we want to use @code{map} as a tag even though Racket is using it for its own function called @racket[map]. First, we invent a command name that doesn't conflict. Let's call it @id{my-map}. As you learned above, Pollen will treat a new command name as a tag function by default:

@codeblock|{
#lang pollen
◊my-map{How I would love this to be a map.}
}|

@repl-output{'(my-map "How I would love this to be a map.")}


But @id{my-map} is not the tag we want. We need to define @id{my-map} to be a tag function for @id{map}. We can do this with the Pollen helper @racket[default-tag-function]. That function lives in @racketmodname[pollen/tag], so we @racket[require] that too:


@codeblock|{
#lang pollen
◊(require pollen/tag)
◊(define my-map (default-tag-function 'map))
◊my-map{How I would love this to be a map.}
}|

@repl-output{'(map "How I would love this to be a map.")}

Problem solved.



@subsubsection{Inserting the value of a variable}

A Pollen command name usually refers to a function, but it can also refer to a @italic{variable}, which is a data value. Once you define the variable, you can insert it into your source by using the ◊ notation without any other arguments:

@codeblock|{
#lang pollen
◊(define foo "bar")
The value of foo is ◊foo
}|

@repl-output{The value of foo is bar}


Be careful — if you include arguments, even blank ones, Pollen will treat the command name as a function. For instance, this next example won't work, because a variable is not a function:


@codeblock|{
#lang pollen
◊(define foo "bar")
The value of foo is ◊foo[]
}|


@errorblock{application: not a procedure;
expected a procedure that can be applied to arguments
  given: "bar"
  arguments...: [none]}


To understand what happens here, recall the relationship between Pollen's command styles. The Pollen-style command @code{◊foo[]} becomes the Racket-style command @code{(foo)}, which after variable substitution becomes @code{("bar")}. If you try to evaluate @code{("bar")} — e.g., in DrRacket — you'll get the same error.

The reason we can simply insert @code{◊foo} into the text body of another Pollen command is that the variable @code{foo} holds a string (i.e., a text value).


In preprocessor source files, Pollen will convert a variable to a string in a sensible way. For instance, numbers are easily converted:

@codeblock|{
#lang pollen
◊(define zam 42)
The value of zam is ◊zam
}|

@repl-output{The value of zam is 42}

@margin-note{In an unsaved DrRacket file, or a file without a special Pollen source extension, the @tt{#lang pollen} designation invokes the Pollen preprocessor by default. You can explicitly invoke preprocessor style by starting a file with @tt{#lang pollen/pre}. See also @secref["Preprocessor___pp_extension_"].}

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
The value of zam is ◊(string-join (map number->string zam) " and ")
}|

@repl-output{The value of zam is 1 and 2 and 3}

Pollen will still produce an error if you try to convert an esoteric value to a string. Here, @code{zam} is the addition function (@racket[+]):

@codeblock|{
#lang pollen
◊(define zam +)
The value of zam is ◊zam
}|

@errorblock{pollen: Can't convert procedure #<procedure:+> to string}

In Pollen markup, the result is different. A Pollen markup file makes an X-expression, not text, so Pollen doesn't perform @italic{any} automatic text conversion — that's your job. Suppose we take the example above — which worked with the Pollen preprocessor — and change the language to @racketmodname[pollen/markup]:

@codeblock|{
#lang pollen/markup
◊(define zam (list 1 2 3))
The value of zam is ◊zam
}|

This time, the file will produce an error:

@errorblock{
  pollen markup error: in '(root "The value of zam is " (1 2 3)), '(1 2 3) is not a valid element (must be txexpr, string, symbol, XML char, or cdata)
}

But the second example above, with the explicit conversion using @racket[string-join], does work in Pollen markup, because strings are valid X-expressions:

@codeblock|{
#lang pollen/markup
◊(require racket/string)
◊(define zam (list 1 2 3))
The value of zam is ◊(string-join (map number->string zam) " and ")
}|

@repl-output{'(root "The value of zam is " "1 and 2 and 3")}

@margin-note{See @secref["File_formats"] for more about the differences between Pollen dialects.}

One special case to know about. In the examples above, there's a word space between the variable and the other text. But suppose you need to insert a variable into text so that there's no space in between. The simple ◊ notation won't work, because it won't be clear where the variable name ends and the text begins. 

For instance, suppose we want to use a  variable @code{edge} next to the string @code{px}:

@codeblock|{
#lang pollen
◊(define edge 100)
p { margin-left: ◊edgepx; }
}|

@errorblock{pollen: Can't convert procedure #<procedure:pollen-tag:edgepx> to string}

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




@subsubsection{Inserting metas}

@italic{Metas} are key–value pairs embedded in a source file that are not included in the main output when the source is compiled. Rather, they're gathered and exported as a separate @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{hash table} called, unsurprisingly, @id{metas}. This hash table is a good place to store information about the document that you might want to use later (for instance, a list of topic categories that the document belongs to).

@margin-note{Pollen occasionally uses metas internally. For instance, the @racket[get-template-for] function will look in the metas of a source file to see if a template is explicitly specified. The @racketmodname[pollen/core] module also contains functions for working with metas, such as @racket[select-from-metas].}

To make a meta, you create a tag with the special @racket[define-meta] name, followed by a key name, and then a value that's a single X-expression:

@codeblock{
#lang pollen

◊define-meta[dog]{Roxy} ; Pollen-style syntax
◊some-tag[#:foo "bar"]{Normal tag}
◊(define-meta cat "Chopper") ; equivalent Racket-style syntax
◊some-tag[#:zim "zam"]{Another normal tag}
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

The only key that's automatically defined in every meta table is @racket['#,pollen-here-path-key], which is the absolute path to the source file. (In this case, because the file hasn't been saved, you'll see the @val{unsaved-editor} name instead.) 

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


It doesn't matter how many metas you put in a source file, nor where you put them. They'll all be extracted into the @id{metas} hash table. The order of the metas is not preserved (because order is not preserved in a hash table). But if you have two metas with the same key, the later one will supersede the earlier one:

@codeblock{
#lang pollen
◊define-meta[dog]{Roxy}
◊(define-meta dog "Lex")
}

Though there are two metas named @id{dog}, only the second one persists:

@terminal{
> metas
'#hasheq((dog . "Lex") (here-path . "unsaved-editor"))
}

What if you want to use a sequence of X-expression elements as a meta value? You can convert them into a single X-expression by wrapping them in a containing tag. You can use a new tag, or even just the @racket[\@] splicing tag:

@codeblock|{
#lang pollen/markup
◊(define-meta title ◊@{Conclusion to ◊em{Infinity War}})

The title is ◊(select-from-metas 'title metas)
}|

@repl-output{'(root "The title is " "Conclusion to " (em "Infinity War"))}


To save a few keystrokes, you can consolidate multiple key–value pairs into one @racket[define-meta] form. So this:

@codeblock{
#lang pollen
◊(define-meta dog "Roxy")
◊(define-meta cat "Chopper")
◊(define-meta ape "Koko") 
}

Is the same as this:

@codeblock{
#lang pollen
◊(define-meta dog "Roxy"
              cat "Chopper"
              ape "Koko")
}

In both cases, the resulting metas look like this:

@terminal{
> metas
'#hasheq((ape . "Koko") (cat . "Chopper") (dog . "Roxy") (here-path . "unsaved editor"))
}


@subsubsection{Retrieving metas}

The @id{metas} hashtable is available immediately within the body of your source file. You can use @racket[select] to get values out of @id{metas}.

@codeblock{
#lang pollen
◊(define-meta dog "Roxy")
◊(select 'dog metas)
}

@repl-output{Roxy}

@id{metas} is an immutable hash, so you can also use immutable-hash functions, like @racket[hash-ref]:

@codeblock{
#lang pollen
◊(define-meta dog "Roxy")
◊(hash-ref metas 'dog)
}

@repl-output{Roxy}


Because the metas are collected first, you can actually invoke a meta before you define it:

@codeblock{
#lang pollen
◊(select 'dog metas)
◊(define-meta dog "Roxy")
◊(define-meta dog "Spooky")
}

@repl-output{Spooky}

This can be useful for setting up fields that you want to include in @id{metas} but also have visible in the body of a document, like a title.

@codeblock{
#lang pollen/markup
◊(define-meta title "The Amazing Truth")
◊h1{◊(select 'title metas)}
}

The result of this file will be:

@repl-output{'(root (h1 "The Amazing Truth"))}

And the metas:
@terminal{
> metas
'#hasheq((title . "The Amazing Truth") (here-path . "unsaved-editor"))
}


@bold{Pro tip}: Within Pollen, the fastest way to get a @id{metas} hashtable from another source file is to use @racket[cached-metas].

@bold{Pro tip #2}: Outside Pollen, the @id{metas} hashtable is available when you import a Pollen source file in the usual way, but it's also made available through a submodule called, unsurprisingly, @id{metas}.

@codeblock{
#lang racket/base
(require "path/to/your-pollen-source") ; doc and metas and everything else
(require (submod "path/to/your-pollen-source" metas)) ; just metas
}

The @id{metas} submodule gives you access to the @id{metas} hashtable @italic{without} compiling the rest of the file. So if you need to harvest metas from a set of source files — for instance, page titles (for a table of contents) or categories — using @racket[require] with the submodule will be faster.

@bold{Pro tip #3}: Within a tag function, you can access the metas of the source currently being evaluated with @racket[current-metas].

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

@subsection{The Racket arguments}

The middle part of a Pollen-style command contains the @italic{Racket arguments} @litchar{[}between square brackets.@litchar{]} Most often, you'll see these used to pass extra information to commands that operate on text.

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


But that's a lot of parentheses to think about. So here's the easy way. Whenever you use a tag function, there's a shortcut for inserting attributes. You can enter them as a series of @italic{keyword arguments} between the Racket-argument brackets. The only caveat is that the values for these keyword arguments have to be strings. So taken together, they look like this:

@codeblock|{
#lang pollen
◊title[#:class "red" #:id "first"]{The Beginning of the End}
}|

@repl-output{'(title ((class "red") (id "first")) "The Beginning of the End")}

The string arguments can be any valid Racket expressions that produce strings. For instance, this will also work:

@codeblock|{
#lang pollen
◊title[#:class (number->string (* 6 7)) #:id "first"]{The Beginning of the End}
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

@bold{Pro tip}: See also @racket[define-tag-function], which automatically converts keyword arguments into attributes before they reach your function:

@codeblock|{
#lang pollen
◊(require pollen/tag)
◊(define-tag-function (heading attrs elems)
  (define level (cadr (assq 'level attrs)))
  `(,(string->symbol (format "h~a" level)) ,@elems))
 
◊heading[#:level 1]{Major league}
◊heading[#:level 2]{Minor league}
◊heading[#:level 6]{Trivial league}
}|

@repl-output{'(h1 "Major league")
'(h2 "Minor league")
'(h6 "Trivial league")
}


@subsection[#:tag "the-text-body"]{The text body}

The third part of a Pollen-style command is the text body. The text body @litchar{@"{"}appears between curly braces@litchar{@"}"}. It can contain any text you want. The text body can also contain other Pollen commands with their own text body. And they can contain other Pollen commands ... and so on, all the way down.

@codeblock|{
#lang pollen
◊div{Do it again. ◊div{And again. ◊div{And yet again.}}}
}|

@repl-output{'(div "Do it again. " (div "And again. " (div "And yet again.")))}

Three things to know about the text body.

First, the only character that needs special handling in the text body is the lozenge @litchar{◊}. A lozenge ordinarily marks a new command. So if you want an actual lozenge to appear in the text, you have to escape it by typing @litchar{◊"◊"}.

@codeblock|{
#lang pollen
◊definition{This is the lozenge: ◊"◊"}
}|

@repl-output{'(definition "This is the lozenge: ◊")}

Second, the whitespace-trimming policy. Here's the short version: if there's a newline at either end of the text body, it is trimmed, and whitespace at the end of each line is selectively trimmed in an intelligent way. So this text body, with newlines on the ends:

@codeblock|{
#lang pollen
◊div{
Roomy!
     
I agree.
}
}|

@repl-output{'(div "Roomy!" "\n" "\n" "I agree.")}

Yields the same result as this one, without the newlines:

@codeblock|{
#lang pollen
◊div{Roomy!
     
I agree.}
}|

@repl-output{'(div "Roomy!" "\n" "\n" "I agree.")}

For the long version, please see @secref["Spaces__Newlines__and_Indentation"
         #:doc '(lib "scribblings/scribble/scribble.scrbl")].


Third, within a multiline text body, newline characters become individual strings that are not merged with adjacent text. So what you end up with is a list of strings, not a single string. That's why in the last example, we got this:

@repl-output{'(div "Roomy!" "\n" "\n" "I agree.")}

Instead of this:

@repl-output{'(div "Roomy!\n\nI agree.")}

Under most circumstances, these two tagged X-expressions will behave the same way. The biggest exception is with functions. A function that might operate on a multiline text body needs to be able to handle an indefinite number of strings. For instance, this @code{jejune} function only accepts a single argument. It will work with a single-line text body, because that produces a single string:

@codeblock|{
#lang pollen
◊(define (jejune text)
   `(jejune ,text))
◊jejune{Irrational confidence}
}|

@repl-output{'(jejune "Irrational confidence")}

But watch what happens with a multiline text body:

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


@section{Adding Pollen-style commands to a Racket file}

@defmodulelang[pollen/mode]

Just as you can embed any Racket-style command in a Pollen source file, you can go the other way and embed Pollen-style commands in a Racket file. Just insert @racketmodname[pollen/mode] in the @tt{#lang} line at the top of your source:

@fileblock["pollen.rkt" @codeblock{
#lang pollen/mode racket/base
(require pollen/tag)

(define link (default-tag-function 'a))

(define (home-link)
  (link #:href "index.html" "Click to go home"))

(define (home-link-pollen-style)
  ◊link[#:href "index.html"]{Click to go home})

}]

Here, both @tt{(home-link)} and @tt{(home-link-pollen-style)} will produce the same X-expression as a result:

@terminal{'(a ((href "index.html")) "Click to go home")}

Of course, you can use @racketmodname[pollen/mode] in any Racket source file, not just @filepath{pollen.rkt}. 

@bold{Major caveat}: @racketmodname[pollen/mode] only works with Pollen's default command character, namely the lozenge (@litchar{◊}). If you've overridden this command character in your @filepath{pollen.rkt} file, your custom command character will work everywhere @italic{except} in @racketmodname[pollen/mode]. This limitation is necessary to prevent the intractable situation where @filepath{pollen.rkt} relies on @racketmodname[pollen/mode], but @racketmodname[pollen/mode] relies on a config setting in @filepath{pollen.rkt}.

Also keep in mind that @racketmodname[pollen/mode] is just a syntactic convenience. It doesn't change any of the underlying semantics of your Racket source file. Your Pollen-style commands are being translated into Racket commands and compiled along with everything else.

Another good way to use Pollen-style commands in Racket is for unit tests with @racketmodname[rackunit]. With @racketmodname[pollen/mode], you can write your unit tests in Pollen style or Racket style (or mix them). This makes it easy to verify that Pollen-style commands will turn into the Racket values that you expect:

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
