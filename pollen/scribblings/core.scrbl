#lang scribble/manual

@(require scribble/eval pollen/cache pollen/setup (for-label racket (except-in pollen #%module-begin) pollen/render txexpr xml pollen/pagetree sugar/coerce pollen/core pollen/setup))
@(require "mb-tools.rkt")

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/core xml))

@title{Core}

@defmodule[pollen/core]

These functions are automatically imported into every Pollen source file (meaning, as if they had been included in your @filepath{pollen.rkt}).



@section{Metas}

The only key that's automatically defined in every meta table is @racket['#,pollen-here-path-key], which holds the absolute path to the source file. For instance, you could retrieve this value with @racket[(select-from-metas '#,pollen-here-path-key metas)].
For a full introduction to metas, see @secref["Inserting_metas"].


@defform[(define-meta name value)]
Add @racket[_value] to the metas of the current document, using @racket[_name] as the key.

You can retrieve a meta value — even in the same document where you define it — with @racket[(select-from-metas _name metas)].




@section{Splicing}

@defform[(\@ arg ...)]
The splicing tag signals that a list should be merged into its containing expression. The splicing tag is @racket['\@].

@examples[#:eval my-eval
(module splicer pollen/markup
'(div "one" (\@ "two" "three") "four"))
(require 'splicer)
doc
]

The splicing tag is useful when you want to return a list of X-expressions in a situation where you can only return one. For instance, @secref["Tag_functions"] can only return one X-expression. But if we wrap the list of X-expressions in a splicing tag, they behave like a single X-expression. Later, Pollen will merge the list elements into the surrounding expression (as shown above).

@examples[#:eval my-eval
(require pollen/tag)

(code:comment @#,t{wrong: function returns a list of X-expressions})
(define-tag-function (multi attrs elems)
  '("foo" "bar"))

(code:comment @#,t{right: function returns a list of X-expressions})
(code:comment @#,t{as elements inside a splicing tag})
(define-tag-function (multi2 attrs elems)
  '(\@ "foo" "bar"))
]


Though the splicing tag is cosmetically identical to the abbreviated notation of @litchar{@"@"} for @racket[unquote-splicing], and has a similar purpose, it's not the same thing. The splicing tag isn't a variable — it's just a symbol that Pollen treats specially when generating output.


@defform[(when/splice condition pollen-args)]
If @racket[_condition] is true, put the @racket[_pollen-args] into the document. Within a template file, usually invoked like so:

@verbatim{◊when/splice[@racketvarfont{condition}]{The text to insert.}}

The inserted text can contain its own nested Pollen commands.

@racket[when/splice] can be more convenient than @racket[when], because @racket[when] will only use the last argument between the curly braces. @racket[when/splice], by contrast, treats everything between the curly braces as a block.


@deftogether[(
@defform[(for/splice (for-clause ...) body-or-break ... body)]
@defform[(for*/splice (for-clause ...) body-or-break ... body)])]
Like @racket[for/list] and @racket[for*/list], but  the resulting list is spliced into the document.

@history[#:added "1.4"]


@section{Data helpers}

Functions for retrieving data out of Pollen source files. These are not the only options – you can, of course, use any of the usual Racket functions.


@defproc[
(get-doc
[doc-source (or/c pagenode? pathish?)])
(or/c txexpr? string?)]
Retrieve the @racket[doc] export from @racket[_doc-source], which can be either a path, path string, or pagenode that can be resolved into a source path. If @racket[_doc-source] cannot be resolved, raise an error.

If @racket[_doc-source] is a relative path or pagenode, it is treated as being relative to @racket[current-project-root]. If that's not what you want, you'll need to convert it explicitly to a complete-path (e.g., with @racket[path->complete-path] or @racket[->complete-path]).

If @racket[setup:main-export] has been overridden with a project-specific value, then that is retrieved instead.


@defproc[
(get-metas
[meta-source (or/c pagenode? pathish?)])
hash?]
Retrieve the @racket[metas] export from @racket[_meta-source], which can be either a path, path string, or pagenode that can be resolved into a source path. If @racket[_meta-source] cannot be resolved, raise an error.

If @racket[_meta-source] is a relative path or pagenode, it is treated as being relative to @racket[current-project-root]. If that's not what you want, you'll need to convert it explicitly to a complete-path (e.g., with @racket[path->complete-path] or @racket[->complete-path]).

If @racket[setup:meta-export] has been overridden with a project-specific value, then that is retrieved instead.


@deftogether[(

@defproc[
(select
[key symbolish?]
[value-source (or/c hash? txexpr? pagenode? pathish?)])
(or/c #f xexpr?)]

@defproc[
(select*
[key symbolish?]
[value-source (or/c hash? txexpr? pagenode? pathish?)])
(or/c #f (listof xexpr?))]

)]
Find matches for @racket[_key] in @racket[_value-source]. The @racket[_value-source] can be 1) a hashtable of @racket[metas], 2) a tagged X-expression representing a @racket[doc], or 3) a pagenode or path that identifies a source file that provides @racket[metas] and @racket[doc]. In that case, first look for @racket[_key] in @code{metas} (using @racket[select-from-metas]) and then in @code{doc} (using @racket[select-from-doc]). 

With @racket[select], you get the first result; with @racket[select*], you get them all. 

In both cases, you get @racket[#f] if there are no matches.

Note that if @racket[_value-source] is a relative path or pagenode, it is treated as being relative to @racket[current-project-root]. If that's not what you want, you'll need to convert it explicitly to a complete-path (e.g., with @racket[path->complete-path] or @racket[->complete-path]).

@examples[#:eval my-eval
(module nut-butters pollen/markup
'(div (question "Flavor?")
  (answer "Cashew") (answer "Almond")))
(code:comment @#,t{Import doc from 'nut-butters submodule})
(require 'nut-butters)
(select 'question  doc)
(select 'answer  doc)
(select* 'answer  doc)
(select 'nonexistent-key doc)
(select* 'nonexistent-key doc)
]


@defproc[
(select-from-doc
[key symbolish?]
[doc-source (or/c txexpr? pagenodeish? pathish?)])
(or/c #f (listof xexpr?))]
Look up the value of @racket[_key] in @racket[_doc-source]. The @racket[_doc-source] argument can be either 1) a tagged X-expression representing a @racket[doc] or 2) a pagenode or source path that identifies a source file that provides @racket[doc]. If no value exists for @racket[_key], you get @racket[#f].

Note that if @racket[_doc-source] is a relative path or pagenode, it is treated as being relative to @racket[current-project-root]. If that's not what you want, you'll need to convert it explicitly to a complete-path (e.g., with @racket[path->complete-path] or @racket[->complete-path]).

@examples[#:eval my-eval
(module gelato pollen/markup
'(div (question "Flavor?")
  (answer "Nocciola") (answer "Pistachio")))
(code:comment @#,t{Import doc from 'gelato submodule})
(require 'gelato)
(select-from-doc 'question  doc)
('answer . select-from-doc . doc)
(select-from-doc 'nonexistent-key doc)
]



@defproc[
(select-from-metas
[key symbolish?]
[meta-source (or/c hash? pagenodeish? pathish?)])
any/c]
Look up the value of @racket[_key] in @racket[_meta-source]. The @racket[_meta-source] argument can be either 1) a hashtable representing @racket[metas] or 2) a pagenode or source path that identifies a source file that provides @racket[metas]. If no value exists for @racket[_key], you get @racket[#f]. 

Note that if @racket[_meta-source] is a relative path or pagenode, it is treated as being relative to @racket[current-project-root]. If that's not what you want, you'll need to convert it explicitly to a complete-path (e.g., with @racket[path->complete-path] or @racket[->complete-path]).

@examples[#:eval my-eval
(define metas (hash 'template "sub.xml.pp" 'target "print"))
(select-from-metas 'template  metas)
('target . select-from-metas . metas)
(select-from-metas 'nonexistent-key metas)
]

@section[#:tag "core"]{Parameters}

@defparam[current-metas val (or/c #f hash?) #:value #f]
Holds the @racket[metas] of the current Pollen source. In tag functions, rather than pass @racket[metas] as an argument, you can refer to @racket[(current-metas)] within the body of the function. Likewise, if your tag function calls other tag functions, they can all invoke @racket[(current-metas)] instead of passing the value around.

@racket[(current-metas)] will also work in templates, holding the @racket[metas] of the source currently being rendered into the template. 

The default value is @racket[#f]. This means that no metas value is available. It's your responsibility to handle this circumstance sensibly.

@history[#:added "1.4"]
